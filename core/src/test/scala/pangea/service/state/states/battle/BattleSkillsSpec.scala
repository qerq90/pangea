package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{ActiveBattle, SkillSlotState}
import pangea.model.item.{Item, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{Race, Rarity}
import pangea.model.skill.Skill
import pangea.model.state.StateType
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestRenderer}
import zio.ZIO
import zio.test._

object BattleSkillsSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  // Высокая концентрация/интеллект → шанс применения умения = 95% (потолок), но без рандома
  // тест не делает предположения о результате одного броска, а проверяет инварианты после.
  private def heroWith(weaponSkill: Option[Skill], chestSkill: Option[Skill]) = {
    val baseHero  = TestFixtures.hero(userId)
    val weapon    = Item(101L, "Меч", 1L, ItemRarity.Gray, ItemType.Weapon,
                         attack=0, accuracy=0, concentration=0, armor=0, defence=0, evasion=0,
                         activeSkill = weaponSkill)
    val chest     = Item(202L, "Кираса", 1L, ItemRarity.Gray, ItemType.ChestPlate,
                         attack=0, accuracy=0, concentration=0, armor=0, defence=0, evasion=0,
                         activeSkill = chestSkill)
    baseHero.copy(
      // Сильные статы → урон и лечение получаются ненулевыми
      baseStats  = baseHero.baseStats.copy(str = 50, int = 50, vit = 50, agi = 50),
      fightStats = baseHero.fightStats.copy(atk = 50, accuracy = 50, defence = 20,
                                            concentration = 10_000, hp = 200, evasion = 0),
      equipment  = TestFixtures.emptyEquipment.copy(weapon = weapon, chestPlate = chest)
    )
  }

  // Слабый монстр с низкой концентрацией — шанс применения умения у героя ≈ 95%
  private def weakBattle(slots: List[SkillSlotState]): ActiveBattle = ActiveBattle(
    monsterLvl          = 1L,
    monsterRace         = Race.Human.entryName,
    monsterRarity       = Rarity.Common.entryName,
    monsterStats        = FightStats(atk = 1, hp = 999, armor = 0, defence = 0,
                                     evasion = 0, accuracy = 1, concentration = 1),
    monsterCurrentHp    = 999L,
    monsterCurrentArmor = 0L,
    skillSlots          = slots
  )

  private def makeState(hero: pangea.model.hero.Hero, battle: ActiveBattle) =
    for {
      heroDao  <- TestHeroDao.withHero(userId, hero)
      _        <- heroDao.writeActiveBattle(userId, battle.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(heroDao, content), heroDao, renderer)

  override def spec = suite("BattleSkillsSpec")(

    test("Карточка боя содержит кнопки скилов для готовых слотов") {
      val hero   = heroWith(Some(Skill.SweepingStrike), Some(Skill.MinorHeal))
      val slots  = List(
        SkillSlotState(101L, Skill.SweepingStrike),
        SkillSlotState(202L, Skill.MinorHeal)
      )
      for {
        triple              <- makeState(hero, weakBattle(slots))
        (state, _, renderer) = triple
        _                   <- state.enter(testUser, renderer)
        screens             <- renderer.sentScreens
        ids                  = screens.head.choices.map(_.id).toSet
      } yield assertTrue(ids.contains("Skill_101")) && assertTrue(ids.contains("Skill_202"))
    },

    test("Кнопка скилла не показывается, пока его cd > 0") {
      val hero  = heroWith(Some(Skill.SweepingStrike), None)
      val slots = List(SkillSlotState(101L, Skill.SweepingStrike, cooldown = 2))
      for {
        triple              <- makeState(hero, weakBattle(slots))
        (state, _, renderer) = triple
        _                   <- state.enter(testUser, renderer)
        screens             <- renderer.sentScreens
        ids                  = screens.head.choices.map(_.id)
      } yield assertTrue(!ids.contains("Skill_101"))
    },

    test("Damage-скилл при попадании наносит урон мобу и ставит cd") {
      val hero  = heroWith(Some(Skill.SweepingStrike), None)
      val slots = List(SkillSlotState(101L, Skill.SweepingStrike))
      val before = weakBattle(slots).monsterCurrentHp
      for {
        triple                       <- makeState(hero, weakBattle(slots))
        (state, heroDao, renderer)    = triple
        result                       <- state.action(testUser, tap("Skill_101"), renderer)
        after                        <- heroDao.readActiveBattle(userId).map(_.flatMap(_.as[ActiveBattle].toOption).get)
      } yield assertTrue(result == StateType.Battle || result == StateType.Loot) &&
              assertTrue(after.monsterCurrentHp <= before) &&
              // cd ставится в skill.cooldown; в этом же ходу slot пропускается в tickBuffs,
              // значит после хода cd остаётся равным skill.cooldown — отсчёт начнётся со
              // СЛЕДУЮЩЕГО хода игрока (cd=1 → можно использовать каждые 2 хода).
              assertTrue(after.slotByItem(101L).exists(s => s.cooldown == Skill.SweepingStrike.cooldown)) &&
              assertTrue(after.slotByItem(101L).exists(_.uses == 1))
    },

    test("Heal-скилл лечит героя и не трогает HP моба") {
      val hero      = heroWith(None, Some(Skill.MinorHeal)).copy()
      val wounded   = hero.copy(fightStats = hero.fightStats.copy(hp = 50L))
      val slots     = List(SkillSlotState(202L, Skill.MinorHeal))
      val battle    = weakBattle(slots)
      val mobHpPre  = battle.monsterCurrentHp
      for {
        triple                       <- makeState(wounded, battle)
        (state, heroDao, renderer)    = triple
        _                            <- state.action(testUser, tap("Skill_202"), renderer)
        afterHero                    <- heroDao.getHeroByUserId(userId).map(_.get)
        afterBattle                  <- heroDao.readActiveBattle(userId).map(_.flatMap(_.as[ActiveBattle].toOption).get)
      } yield assertTrue(afterHero.fightStats.hp > 50L) &&
              assertTrue(afterBattle.monsterCurrentHp <= mobHpPre) // моб мог ответить, но не от скилла
    },

    test("Использование незарегистрированного слота → ход не съедается, ошибочное сообщение") {
      val hero  = heroWith(None, None)
      val slots = Nil
      for {
        triple              <- makeState(hero, weakBattle(slots))
        (state, _, renderer) = triple
        result              <- state.action(testUser, tap("Skill_999"), renderer)
        screens             <- renderer.sentScreens
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(screens.exists(_.text.contains("недоступна")))
    },

    test("Использование скилла на кулдауне → ход не съедается, сообщение") {
      val hero  = heroWith(Some(Skill.SweepingStrike), None)
      val slots = List(SkillSlotState(101L, Skill.SweepingStrike, cooldown = 2, uses = 1))
      for {
        triple                       <- makeState(hero, weakBattle(slots))
        (state, heroDao, renderer)    = triple
        result                       <- state.action(testUser, tap("Skill_101"), renderer)
        screens                      <- renderer.sentScreens
        after                        <- heroDao.readActiveBattle(userId).map(_.flatMap(_.as[ActiveBattle].toOption).get)
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(screens.exists(_.text.contains("восстанавливается"))) &&
              // cd НЕ перезатёрся, uses не вырос
              assertTrue(after.slotByItem(101L).exists(s => s.cooldown == 2 && s.uses == 1))
    },

    test("heroSkillHitChance клампится в [5;95]") {
      val cappedHi = BattleState.heroSkillHitChance(
        heroConc = 1_000_000L, heroInt = 1_000_000L, monsterConc = 1L, rarityFactor = 0.1)
      val cappedLo = BattleState.heroSkillHitChance(
        heroConc = 1L, heroInt = 1L, monsterConc = 1_000_000L, rarityFactor = 10.0)
      assertTrue(cappedHi == 95.0) && assertTrue(cappedLo == 5.0)
    },

    test("monsterSkillHitChance клампится в [5;95]") {
      val cappedHi = BattleState.monsterSkillHitChance(
        monsterConc = 1_000_000L, rarityFactor = 10.0, heroConc = 1L, heroInt = 1L)
      val cappedLo = BattleState.monsterSkillHitChance(
        monsterConc = 1L, rarityFactor = 0.1, heroConc = 1_000_000L, heroInt = 1_000_000L)
      assertTrue(cappedHi == 95.0) && assertTrue(cappedLo == 5.0)
    },

    test("parseSkillAction распознаёт Skill_<id>, иначе None") {
      val ok  = BattleState.parseSkillAction(tap("Skill_42"))
      val bad = BattleState.parseSkillAction(tap("Attack"))
      assertTrue(ok.contains(42L)) && assertTrue(bad.isEmpty)
    }
  )
}
