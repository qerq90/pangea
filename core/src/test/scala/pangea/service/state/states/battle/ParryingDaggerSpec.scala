package pangea.service.state.states.battle

import io.circe.syntax.EncoderOps
import pangea.domain.Rng
import pangea.engine.SceneContent
import pangea.generator.item.ItemGenerator
import pangea.model.battle.SoloPveBattle
import pangea.model.hero.Hero
import pangea.model.item.{Grip, Item, ItemDetails, ItemType, Rarity => ItemRarity}
import pangea.model.monster.{Monster, Race, Rarity}
import pangea.model.rune.{Rune, RuneStone}
import pangea.model.skill.Skill
import pangea.model.stats.FightStats
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.state.UserAction
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestItemRepository, TestRenderer}
import zio.ZIO
import zio.test.TestRandom
import zio.test._

/** Парирующий кинжал: одноручное с 40% статов, зато с «Контрударом» — увернулся
  * в этом раунде от удара, и тут же бьёшь в ответ наверняка. */
object ParryingDaggerSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))
  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private val dagger: Item =
    Item(401L, "Парирующий кинжал", 1L, ItemRarity.Gray, ItemType.Weapon,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0,
      grip = Some(Grip.Parrying), details = ItemDetails.HandSkill(Skill.Counterstrike))

  private def hero: Hero = {
    val base = TestFixtures.hero(userId)
    base.copy(
      lvl        = 10L,
      fightStats = FightStats(atk = 100L, hp = 500000L, armor = 0, defence = 0,
                              evasion = 0, accuracy = 9999, energy = 10_000L),
      baseStats  = base.baseStats.copy(str = 1, vit = 5000),
      equipment  = TestFixtures.emptyEquipment.copy(weapon = dagger))
  }

  private val monster: Monster =
    Monster(0L, 10L, Race.Orc, Rarity.Common,
      FightStats(atk = 100, hp = 100000, armor = 0, defence = 0, evasion = 0, accuracy = 9999, energy = 0))

  private def makeState(h: Hero, b: SoloPveBattle) =
    for {
      dao      <- TestHeroDao.withHero(userId, h)
      _        <- dao.writeActiveBattle(userId, b.asJson)
      renderer <- TestRenderer.make
      content  <- ZIO.attempt(SceneContent.load())
    } yield (BattleState(dao, TestInventoryRepository.accepting, TestItemRepository.make, content), dao, renderer)

  private def battleOf(dao: TestHeroDao) =
    dao.readActiveBattle(userId).map(_.flatMap(_.as[SoloPveBattle].toOption).get)

  private def texts(r: TestRenderer) = r.sentScreens.map(_.map(_.text).mkString("\n"))

  private def generated(seed: Long, grip: Option[Grip], rarity: ItemRarity = ItemRarity.Blue): Item =
    ItemGenerator.createItemOfType(ItemType.Weapon, 30L, rarity, Rng(seed), grip)._1

  override def spec = suite("Парирующий кинжал")(

    test("40% статов того же оружия, одноручный, имя своё, умение — Контрудар, гнёзд до двух") {
      val pairs = (1L to 200L).map(s => generated(s, Some(Grip.Parrying)) -> generated(s, Some(Grip.TwoHanded)))
      def cut(v: Long) = v * 40L / 100L
      val stats  = pairs.forall { case (p, t) => p.attack == cut(t.attack) && p.evasion == cut(t.evasion) && p.armor == cut(t.armor) }
      val named  = pairs.forall { case (p, _) => p.name.split(" ").slice(2, 4).mkString(" ") == "Парирующий Кинжал" }
      val top    = (1L to 100L).map(s => generated(s, Some(Grip.Parrying), ItemRarity.Orange))
      val green  = (1L to 100L).map(s => generated(s, Some(Grip.Parrying), ItemRarity.Green))
      assertTrue(stats && named) &&
      assertTrue(pairs.forall { case (p, _) => !p.twoHanded && p.details == ItemDetails.HandSkill(Skill.Counterstrike) }) &&
      assertTrue(top.forall(_.sockets.size == 2) && green.forall(_.sockets.isEmpty)) &&
      // кинжал — оружие: с ним герой не безоружен
      assertTrue(!TestFixtures.emptyEquipment.copy(leftHand = dagger).unarmed)
    },

    test("Контрудар — не руна") {
      assertTrue(Rune.of(dagger).isEmpty && !RuneStone.all.contains(Rune.Active(Skill.Counterstrike)))
    },

    test("случай больше не ходит по кругу: умение оружия — любое из восьми, у одноручного и у двуручного") {
      val one = (1L to 3000L).map(s => generated(s, Some(Grip.OneHanded))).flatMap(_.activeSkill).toSet
      val two = (1L to 3000L).map(s => generated(s, Some(Grip.TwoHanded))).flatMap(_.activeSkill).toSet
      // Младший бит прежнего генератора менялся через шаг: выбор из двух шёл
      // строго по очереди. Теперь два одинаковых подряд — обычное дело.
      val flips = Iterator.iterate(Rng(7L).pick(List(0, 1)))(x => x._2.pick(List(0, 1))).map(_._1).take(200).toList
      assertTrue(one == Skill.weaponSkills.toSet && two == Skill.weaponSkills.toSet) &&
      assertTrue(flips.sliding(2).exists { case List(a, b) => a == b; case _ => false })
    },

    test("кнопка Контрудара — в ряду действий, справа от «Атаки»") {
      for {
        t <- makeState(hero, SoloPveBattle.from(monster, hero))
        (state, _, r) = t
        _      <- state.enter(testUser, r)
        screen <- r.sentScreens.map(_.last)
      } yield assertTrue(screen.choices.filter(_.row.contains(1)).map(_.id).take(2) == List("Attack", "Skill_401"))
    },

    test("Контрудар — бонусное действие: после стойки ход свой; увернулся — ответ наверняка, попали — ответа нет") {
      def round(mobHitRoll: Int) =
        for {
          t <- makeState(hero, SoloPveBattle.from(monster, hero))
          (state, dao, r) = t
          // Стойка — разброс умения.
          _      <- TestRandom.feedLongs(100L)
          _      <- state.action(testUser, tap("Skill_401"), r)
          mid    <- battleOf(dao)
          // Удар героя — попадание и разброс; бросок моба; разброс ответа или удара моба.
          _      <- TestRandom.feedInts(60, mobHitRoll) *> TestRandom.feedLongs(100L, 100L)
          _      <- state.action(testUser, tap("Attack"), r)
          battle <- battleOf(dao)
          said   <- texts(r)
        } yield (monster.fightStats.hp - battle.monsterCurrentHp, battle, said, mid)
      for {
        dodged <- round(mobHitRoll = 1)
        struck <- round(mobHitRoll = 99)
        (dealt, after, log, mid) = dodged
      } yield assertTrue(mid.heroCounterReady && mid.monsterCurrentHp == monster.fightStats.hp && mid.midRound) &&
              // ответ — сверх своего удара
              assertTrue(log.contains("Контрудар!") && dealt > struck._1) &&
              // стойка отработала и снята; умение — на перезарядке, в этом раунде не тикнуло
              assertTrue(!after.heroCounterReady && after.skillSlots.find(_.itemId == 401L).exists(_.cooldown == 3)) &&
              assertTrue(struck._1 > 0L && !struck._3.contains("Контрудар!"))
    },

    test("Контрудар и по соседу сбоку — но один за раунд") {
      // Герой и моб в паре — на втором месте, соседи — на первом и третьем.
      val b0 = SoloPveBattle.fromGroup(List.fill(3)(monster), hero, Nil)
      val b  = b0.copy(group = b0.group.copy(heroPos = 2, activePos = 2, places = List(1, 3)))
      for {
        t <- makeState(hero, b)
        (state, dao, r) = t
        _     <- TestRandom.feedLongs(100L)
        _     <- state.action(testUser, tap("Skill_401"), r)
        // герой бьёт свою пару; моб в паре попал; первый сосед промахнулся — ответ
        // ему; второй тоже, но ответ уже был
        _     <- TestRandom.feedInts(60, 99, 1, 1) *> TestRandom.feedLongs(100L, 100L, 100L)
        // соседи тоже в досягаемости — цель называем сами: свою пару
        _     <- state.action(testUser, UserAction("", Some("""{"action":"Attack","target":"2"}""")), r)
        after <- battleOf(dao)
        hps    = after.group.others.map(_.currentHp)
      } yield assertTrue(after.monsterCurrentHp < monster.fightStats.hp) &&
              assertTrue(hps.count(_ < monster.fightStats.hp) == 1 && hps.count(_ == monster.fightStats.hp) == 1) &&
              assertTrue(!after.heroCounterReady)
    }
  )
}
