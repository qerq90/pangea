package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.battle.{SoloPveBattle, SkillSlotState}
import pangea.model.item.{Item, ItemDetails, ItemType, Rarity => ItemRarity}
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

  // Много энергии → умения точно применяются (умения всегда попадают, тратят энергию).
  private def heroWith(weaponSkill: Option[Skill], chestSkill: Option[Skill]) = {
    val baseHero  = TestFixtures.hero(userId)
    val weapon    = Item(101L, "Меч", 1L, ItemRarity.Gray, ItemType.Weapon,
                         attack=0, accuracy=0, energy=0, armor=0, defence=0, evasion=0,
                         details = weaponSkill.map(ItemDetails.Weapon).getOrElse(ItemDetails.Plain))
    val chest     = Item(202L, "Кираса", 1L, ItemRarity.Gray, ItemType.ChestPlate,
                         attack=0, accuracy=0, energy=0, armor=0, defence=0, evasion=0,
                         details = chestSkill.map(ItemDetails.Armor).getOrElse(ItemDetails.Plain))
    baseHero.copy(
      // Сильные статы → урон и лечение получаются ненулевыми
      baseStats  = baseHero.baseStats.copy(str = 50, int = 50, vit = 50, agi = 50),
      fightStats = baseHero.fightStats.copy(atk = 50, accuracy = 50, defence = 20,
                                            energy = 10_000, hp = 200, evasion = 0),
      equipment  = TestFixtures.emptyEquipment.copy(weapon = weapon, chestPlate = chest)
    )
  }

  // Слабый монстр — герой безопасно бьёт умениями и проверяет инварианты.
  private def weakBattle(slots: List[SkillSlotState]): SoloPveBattle = SoloPveBattle(
    monsterLvl          = 1L,
    monsterRace         = Race.Human.entryName,
    monsterRarity       = Rarity.Common.entryName,
    monsterStats        = FightStats(atk = 1, hp = 999, armor = 0, defence = 0,
                                     evasion = 0, accuracy = 1, energy = 1),
    monsterCurrentHp    = 999L,
    monsterCurrentArmor = 0L,
    skillSlots          = slots
  )

  private def makeState(hero: pangea.model.hero.Hero, battle: SoloPveBattle) =
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
        after                        <- heroDao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
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
        afterBattle                  <- heroDao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      } yield assertTrue(afterHero.fightStats.hp > 50L) &&
              assertTrue(afterBattle.monsterCurrentHp <= mobHpPre) // моб мог ответить, но не от скилла
    },

    test("Кровавая жатва: герой теряет ~10% HP и наносит урон мобу") {
      val hero    = heroWith(Some(Skill.BloodHarvest), None)
      val wounded = hero.copy(fightStats = hero.fightStats.copy(hp = 200L))
      val slots   = List(SkillSlotState(101L, Skill.BloodHarvest))
      val battle  = weakBattle(slots)
      val mobHp   = battle.monsterCurrentHp
      for {
        triple                       <- makeState(wounded, battle)
        (state, heroDao, renderer)    = triple
        _                            <- state.action(testUser, tap("Skill_101"), renderer)
        afterHero                    <- heroDao.getHeroByUserId(userId).map(_.get)
        afterBattle                  <- heroDao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      } yield // самоурон 10% от 200 = 20 (плюс возможный слабый удар моба) → HP заметно упал
              assertTrue(afterHero.fightStats.hp <= 180L) &&
              assertTrue(afterBattle.monsterCurrentHp < mobHp)
    },

    test("Кровавая жатва: HP-цена списывается, даже когда добивает базовая атака (регресс)") {
      // Моб: hp=1, armor=150. Урон скилла BloodHarvest (≤146 при данных статах)
      // полностью гасится бронёй → моб переживает скилл; базовая атака следом
      // (≥160) гарантированно пробивает остаток брони и добивает моба. Так кил
      // приходится на БАЗОВУЮ атаку — раньше в этой ветке победы 10% HP-цена
      // терялась (не персистилась). Диапазоны урона делают исход детерминированным.
      val hero   = heroWith(Some(Skill.BloodHarvest), None)
      val slots  = List(SkillSlotState(101L, Skill.BloodHarvest))
      val battle = weakBattle(slots).copy(monsterCurrentHp = 1L, monsterCurrentArmor = 150L)
      for {
        triple                       <- makeState(hero, battle)
        (state, heroDao, renderer)    = triple
        result                       <- state.action(testUser, tap("Skill_101"), renderer)
        afterHero                    <- heroDao.getHeroByUserId(userId).map(_.get)
      } yield // добили в тот же ход → Loot; HP = 200 − 10% = 180 (моб не успел ответить)
              assertTrue(result == StateType.Loot) &&
              assertTrue(afterHero.fightStats.hp == 180L)
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
        after                        <- heroDao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(screens.exists(_.text.contains("восстанавливается"))) &&
              // cd НЕ перезатёрся, uses не вырос
              assertTrue(after.slotByItem(101L).exists(s => s.cooldown == 2 && s.uses == 1))
    },

    test("Недостаточно энергии → умение не срабатывает, ход не тратится") {
      val hero    = heroWith(Some(Skill.SweepingStrike), None)
      val drained = hero.copy(fightStats = hero.fightStats.copy(energy = 0L))
      val slots   = List(SkillSlotState(101L, Skill.SweepingStrike))
      val battle  = weakBattle(slots)
      val before  = battle.monsterCurrentHp
      for {
        triple                       <- makeState(drained, battle)
        (state, heroDao, renderer)    = triple
        result                       <- state.action(testUser, tap("Skill_101"), renderer)
        screens                      <- renderer.sentScreens
        after                        <- heroDao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)
      } yield assertTrue(result == StateType.Battle) &&
              assertTrue(screens.exists(_.text.contains("Недостаточно энергии"))) &&
              // моб не получил урона, слот не откатился и не засчитал использование
              assertTrue(after.monsterCurrentHp == before) &&
              assertTrue(after.slotByItem(101L).exists(s => s.cooldown == 0 && s.uses == 0))
    },

    test("monsterSkillHitChance зависит только от редкости и клампится в [5;95]") {
      val cappedHi = BattleState.monsterSkillHitChance(rarityFactor = 10.0)
      val cappedLo = BattleState.monsterSkillHitChance(rarityFactor = 0.1)
      val mid      = BattleState.monsterSkillHitChance(rarityFactor = 1.0)
      assertTrue(cappedHi == 95.0) && assertTrue(cappedLo == 5.0) && assertTrue(mid == 20.0)
    },

    test("parseSkillAction распознаёт Skill_<id>, иначе None") {
      val ok  = BattleState.parseSkillAction(tap("Skill_42"))
      val bad = BattleState.parseSkillAction(tap("Attack"))
      assertTrue(ok.contains(42L)) && assertTrue(bad.isEmpty)
    }
  )
}
