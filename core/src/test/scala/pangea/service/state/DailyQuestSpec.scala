package pangea.service.state

import io.circe.syntax.EncoderOps
import pangea.engine.SceneContent
import pangea.model.hero.Hero
import pangea.model.item._
import pangea.model.monster.Race
import pangea.model.quest._
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.test.{TestFixtures, TestHeroDao, TestInventoryRepository, TestRenderer}
import zio.ZIO
import zio.test._

/** Ежедневные поручения горожан: сутки по Москве, своё поручение у каждого,
  * прогресс — где прибавкой, где по счётчику героя, где прямо по сумке. */
object DailyQuestSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def hero(kills: Long = 0L, rep: Long = 0L): Hero =
    TestFixtures.hero(userId).copy(lvl = 10L, kills = kills, guildReputation = rep)

  /** Полдень 30 сентября 2026 по UTC — заведомо середина московских суток. */
  private val noon = 1790766000000L

  private def dao(h: Hero = hero()) = TestHeroDao.withHero(userId, h)

  private def thing(id: Long, name: String, itemType: ItemType, details: ItemDetails): Item =
    Item(id, name, lvl = 1L, Rarity.Gray, itemType,
      attack = 0, accuracy = 0, energy = 0, armor = 0, defence = 0, evasion = 0, details = details)

  private def relic(id: Long, race: Race): Item =
    thing(id, s"Реликвия ($race)", ItemType.Trophy,
      ItemDetails.Trophy(race.entryName, TrophyKind.Relic))

  private def stone(id: Long, kind: GemKind, grade: Int): Item =
    thing(id, Gem.gradeName(kind, grade), ItemType.Gem, ItemDetails.Gem(Gem(kind, grade)))

  private def herb(id: Long, kind: MaterialKind): Item =
    thing(id, kind.displayName, ItemType.Material, ItemDetails.Material(kind))

  /** Поручение, выданное в обход броска: проверяем механику, а не выбор. */
  private def seed(d: TestHeroDao, npc: DailyNpc, task: DailyTask) =
    DailyQuestLog.save(d, userId, DailyQuests.empty.updated(npc, task))

  override def spec = suite("Ежедневные поручения")(

    test("сутки считаются по Москве: день кончается в полночь по ней, а не по UTC") {
      // 20:59 UTC — ещё сегодня по Москве (23:59), 21:00 UTC — уже завтра.
      val day    = 20000L
      val msk0   = day * DailyRates.DayMs - DailyRates.MoscowOffsetMs   // полночь по Москве
      val before = msk0 - 60000L
      val after  = msk0 + 60000L
      assertTrue(DailyRates.dayOf(before) == day - 1 && DailyRates.dayOf(after) == day) &&
      assertTrue(DailyRates.MoscowOffsetMs == 3L * 60L * 60L * 1000L) &&
      // до новых поручений — меньше суток и больше нуля
      assertTrue(DailyRates.untilNextDay(after) > 0L &&
                 DailyRates.untilNextDay(after) <= DailyRates.DayMs)
    },

    test("у каждого горожанина свой набор, и каждое поручение знает своего") {
      val byNpc = DailyNpc.values.map(n => n -> DailyKind.of(n)).toMap
      val sizes = byNpc.map { case (npc, kinds) => npc.key -> kinds.size }
      assertTrue(DailyNpc.values.size == 4 && DailyKind.values.size == 14) &&
      assertTrue(sizes == Map("rakhadim" -> 3, "richelieu" -> 5, "horn" -> 3, "gustavo" -> 3)) &&
      // ключи уникальны в пределах горожанина, и каждое поручение знает своего
      assertTrue(byNpc.forall { case (npc, kinds) =>
        kinds.map(_.key).distinct.size == kinds.size && kinds.forall(_.npc == npc) }) &&
      // по счётчику героя считаются только два — у Горна
      assertTrue(DailyKind.values.filter(_.snap).map(_.npc).toSet == Set[DailyNpc](DailyNpc.Horn)) &&
      // «принеси» есть у всех, кроме кузнеца: Горну носить нечего
      assertTrue(DailyKind.values.collect { case b: DailyBring => b }.size == 7) &&
      assertTrue(DailyKind.of(DailyNpc.Horn).forall { case _: DailyBring => false; case _ => true })
    },

    test("поручение на день выдаётся один раз: сегодня то же, завтра — заново") {
      for {
        d      <- dao()
        h       = hero()
        first  <- DailyQuestLog.todays(d, h, DailyNpc.Richelieu, noon)
        again  <- DailyQuestLog.todays(d, h, DailyNpc.Richelieu, noon + 3600000L)
        tomorrow <- DailyQuestLog.todays(d, h, DailyNpc.Richelieu, noon + DailyRates.DayMs)
        saved  <- DailyQuestLog.load(d, userId)
      } yield assertTrue(again == first && first.kind.npc == DailyNpc.Richelieu) &&
              assertTrue(tomorrow.day == first.day + 1) &&
              // в записи остаётся одно поручение на горожанина — свежее
              assertTrue(saved.of(DailyNpc.Richelieu).exists(_.day == tomorrow.day))
    },

    test("у «принеси» всегда есть уточнение, и за день оно не меняется") {
      for {
        d     <- dao()
        h      = hero()
        today <- ZIO.foreach(DailyNpc.values.toList)(n => DailyQuestLog.todays(d, h, n, noon))
        again <- ZIO.foreach(DailyNpc.values.toList)(n => DailyQuestLog.todays(d, h, n, noon + 7200000L))
      } yield assertTrue(again == today) &&
              assertTrue(today.forall(t => t.bring.forall(b =>
                if (b.picks.isEmpty) t.pick.isEmpty else t.pick.exists(b.picks.contains))))
    },

    test("редкая трава — заказ нечастый: примерно раз в десять дней") {
      val days = 300
      for {
        d     <- dao()
        h      = hero()
        kinds <- ZIO.foreach((0 until days).toList)(i =>
                   DailyQuestLog.todays(d, h, DailyNpc.Gustavo, noon + i * DailyRates.DayMs).map(_.kind))
        rare   = kinds.count(_ == DailyKind.HerbRare)
      } yield
        // объявленные доли: девять, девять и два из двадцати
        assertTrue(DailyKind.of(DailyNpc.Gustavo).map(_.weight).sum == 20) &&
        assertTrue(DailyKind.HerbRare.weight == 2) &&
        // и на деле примерно так же: от трёх до двадцати процентов дней
        assertTrue(rare * 100 >= days * 3 && rare * 100 <= days * 20) &&
        // прочие поручения при этом не пропадают
        assertTrue(kinds.toSet.size == 3) &&
        // у остальных горожан веса ровные — поручения идут поровну
        assertTrue(DailyKind.values.filter(_.weight != 1).map(_.npc).toSet ==
                   Set[DailyNpc](DailyNpc.Gustavo))
    },

    test("прибавка идёт только взятому поручению и только своему") {
      for {
        d    <- dao()
        h     = hero()
        task  = DailyTask(DailyKind.SellItems, DailyRates.dayOf(noon))
        _    <- seed(d, DailyNpc.Richelieu, task)
        // не взято — прибавлять нечего
        _    <- DailyQuestLog.add(d, userId, DailyNpc.Richelieu, task.kind, 2L)
        idle <- DailyQuestLog.load(d, userId).map(_.of(DailyNpc.Richelieu).get)
        _    <- DailyQuestLog.take(d, h, DailyNpc.Richelieu, noon)
        _    <- DailyQuestLog.add(d, userId, DailyNpc.Richelieu, task.kind, 2L)
        // чужое поручение своего счётчика не двигает
        _    <- DailyQuestLog.add(d, userId, DailyNpc.Richelieu, DailyKind.BankLot, 5L)
        went <- DailyQuestLog.load(d, userId).map(_.of(DailyNpc.Richelieu).get)
      } yield assertTrue(idle.count == 0L && !idle.taken) &&
              assertTrue(went.taken && went.count == 2L)
    },

    test("сданное поручение больше не копит прогресс") {
      for {
        d    <- dao()
        h     = hero()
        _    <- seed(d, DailyNpc.Richelieu, DailyTask(DailyKind.SellJunk, DailyRates.dayOf(noon)))
        task <- DailyQuestLog.take(d, h, DailyNpc.Richelieu, noon)
        _    <- DailyQuestLog.add(d, userId, DailyNpc.Richelieu, task.kind, task.kind.goal)
        full <- DailyQuestLog.load(d, userId).map(_.of(DailyNpc.Richelieu).get)
        _    <- DailyQuestLog.complete(d, userId, DailyNpc.Richelieu, full)
        _    <- DailyQuestLog.add(d, userId, DailyNpc.Richelieu, task.kind, 5L)
        done <- DailyQuestLog.load(d, userId).map(_.of(DailyNpc.Richelieu).get)
      } yield assertTrue(full.ready && !full.done) &&
              assertTrue(done.done && done.count == task.kind.goal)
    },

    test("поручение Горна по убитым считается от мерки, снятой при взятии") {
      val before = hero(kills = 100L)
      for {
        d     <- TestHeroDao.withHero(userId, before)
        // чтобы наверняка выпало счётчиковое — кладём его в запись сами
        _     <- seed(d, DailyNpc.Horn, DailyTask(DailyKind.HornKills, DailyRates.dayOf(noon)))
        taken <- DailyQuestLog.take(d, before, DailyNpc.Horn, noon)
        // герой положил пятерых уже после того, как взялся
        after  = before.copy(kills = 105L)
        _     <- d.insertHero(after)
        seen  <- DailyQuestLog.todays(d, after, DailyNpc.Horn, noon)
      } yield assertTrue(taken.from == 100L && taken.count == 0L) &&
              assertTrue(seen.count == 5L && !seen.ready) &&
              // вчерашние подвиги не в счёт: мерка снята в момент взятия
              assertTrue(DailyKind.HornKills.snap && DailyKind.HornKills.goal == 20L)
    },

    test("сумка сама не пустеет: пока не нажал «Отдать», вещи при герое") {
      val day  = DailyRates.dayOf(noon)
      val task = DailyTask(DailyKind.HerbNamed, day, pick = Some(MaterialKind.Nettle.entryName), taken = true)
      val bag  = List(herb(1L, MaterialKind.Nettle), herb(2L, MaterialKind.Nettle),
                      herb(3L, MaterialKind.Nettle), herb(4L, MaterialKind.Sage))
      for {
        d      <- dao()
        h      <- d.getHeroByUserId(userId).map(_.get)
        c      <- ZIO.attempt(SceneContent.load())
        r      <- TestRenderer.make
        inv     = TestInventoryRepository.withItems(bag)
        _      <- seed(d, DailyNpc.Gustavo, task)
        dlg     = DailyDialog(d, c, DailyNpc.Gustavo, "Gustavo", Some(inv))
        idle   <- dlg.today(h, noon)
        _      <- dlg.show(testUser, idle, noon, r)
        seen   <- r.sentScreens.map(_.last)
        before  = inv.snapshot.map(_.id)
        _      <- dlg.give(testUser, h, noon, r)
        given  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        after  <- dlg.today(h, noon)
      } yield // заглянуть к Густаво — не значит отдать: ни прогресса, ни пропажи в сумке
              assertTrue(idle.count == 0L && !idle.ready && before == List(1L, 2L, 3L, 4L)) &&
              assertTrue(seen.choices.map(_.id) == List("GustavoDailyGive", "GustavoDailyBack")) &&
              // после кнопки ушли ровно две крапивы: третья и шалфей остались при герое
              assertTrue(after.count == 2L && after.ready) &&
              assertTrue(inv.snapshot.map(_.id) == List(3L, 4L)) &&
              assertTrue(given.contains("Густаво забирает"))
    },

    test("отдавать можно по частям, и лишнего горожанин не берёт") {
      val day = DailyRates.dayOf(noon)
      for {
        d     <- dao()
        h     <- d.getHeroByUserId(userId).map(_.get)
        c     <- ZIO.attempt(SceneContent.load())
        r     <- TestRenderer.make
        // заказ на шесть трав, а в сумке пока четыре
        inv    = TestInventoryRepository.withItems((1L to 4L).toList.map(herb(_, MaterialKind.Sage)))
        _     <- seed(d, DailyNpc.Gustavo, DailyTask(DailyKind.HerbsAny, day, taken = true))
        dlg    = DailyDialog(d, c, DailyNpc.Gustavo, "Gustavo", Some(inv))
        _     <- dlg.give(testUser, h, noon, r)
        half  <- dlg.today(h, noon)
        // донёс ещё три — возьмут только недостающие две
        _     <- ZIO.foreachDiscard(5L to 7L)(i => inv.addItem(h.id, herb(i, MaterialKind.Chamomile)).ignore)
        _     <- dlg.give(testUser, h, noon, r)
        full  <- dlg.today(h, noon)
      } yield assertTrue(half.count == 4L && !half.ready && inv.snapshot.isEmpty == false) &&
              assertTrue(full.count == DailyKind.HerbsAny.goal && full.ready) &&
              // седьмая трава осталась при герое: больше, чем просили, не берут
              assertTrue(inv.snapshot.size == 1)
    },

    test("чужого не берут: ни реликвию другой расы, ни целый камень") {
      val day  = DailyRates.dayOf(noon)
      val task = DailyTask(DailyKind.BankRelic, day, pick = Some(Race.Elf.entryName), taken = true)
      for {
        d      <- dao()
        h      <- d.getHeroByUserId(userId).map(_.get)
        c      <- ZIO.attempt(SceneContent.load())
        r      <- TestRenderer.make
        // в сумке чужая реликвия и голова: ни то, ни другое не годится
        inv     = TestInventoryRepository.withItems(List(relic(1L, Race.Orc),
                    thing(2L, "Голова (Эльф)", ItemType.Trophy,
                      ItemDetails.Trophy(Race.Elf.entryName, TrophyKind.Head))))
        _      <- seed(d, DailyNpc.Rakhadim, task)
        dlg     = DailyDialog(d, c, DailyNpc.Rakhadim, "Rakhadim", Some(inv))
        _      <- dlg.give(testUser, h, noon, r)
        refused <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        empty  <- dlg.today(h, noon)
        _      <- inv.addItem(h.id, relic(3L, Race.Elf)).ignore
        _      <- dlg.give(testUser, h, noon, r)
        done   <- dlg.today(h, noon)
      } yield assertTrue(empty.count == 0L && !empty.ready && refused.contains("Пусто")) &&
              assertTrue(inv.snapshot.map(_.id) == List(1L, 2L)) &&
              assertTrue(done.count == 1L && done.ready) &&
              // камень берут только надколотый и только названной породы
              assertTrue(DailyKind.BankGem.accepts(stone(5L, GemKind.Ruby, 1), Some(GemKind.Ruby.entryName))) &&
              assertTrue(!DailyKind.BankGem.accepts(stone(6L, GemKind.Ruby, 3), Some(GemKind.Ruby.entryName))) &&
              assertTrue(!DailyKind.BankGem.accepts(stone(7L, GemKind.Topaz, 1), Some(GemKind.Ruby.entryName)))
    },

    test("сдача закрывает поручение: опыт идёт, завтрашнее не копится") {
      val day  = DailyRates.dayOf(noon)
      val task = DailyTask(DailyKind.HerbNamed, day, count = DailyKind.HerbNamed.goal,
                   pick = Some(MaterialKind.Nettle.entryName), taken = true)
      for {
        d     <- dao()
        h     <- d.getHeroByUserId(userId).map(_.get)
        c     <- ZIO.attempt(SceneContent.load())
        inv    = TestInventoryRepository.withItems(List(herb(1L, MaterialKind.Nettle)))
        _     <- seed(d, DailyNpc.Gustavo, task)
        dlg    = DailyDialog(d, c, DailyNpc.Gustavo, "Gustavo", Some(inv))
        paid  <- dlg.hand(testUser, h, noon)
        after <- d.getHeroByUserId(userId).map(_.get)
        log   <- DailyQuestLog.load(d, userId).map(_.of(DailyNpc.Gustavo).get)
      } yield assertTrue(paid.exists(_._2 == DailyRates.exp(h.lvl)) && log.done) &&
              assertTrue(after.exp >= DailyRates.exp(h.lvl)) &&
              // награда берёт только то, что уже сдано: сумку сдача не трогает
              assertTrue(inv.snapshot.map(_.id) == List(1L))
    },

    test("страже уходит худшее: хороший клинок у героя не заберут") {
      val task  = DailyTask(DailyKind.GuardWeapons, 1L, taken = true)
      val sword = thing(1L, "Меч", ItemType.Weapon, ItemDetails.Plain).copy(rarity = Rarity.Orange)
      val bag   = List(sword,
        thing(2L, "Нож", ItemType.Weapon, ItemDetails.Plain),
        thing(3L, "Палка", ItemType.Weapon, ItemDetails.Plain).copy(rarity = Rarity.White),
        thing(4L, "Тесак", ItemType.Weapon, ItemDetails.Plain).copy(rarity = Rarity.Green),
        thing(5L, "Шлем", ItemType.Helmet, ItemDetails.Plain))
      val gone = task.toGive(bag).map(_.id)
      assertTrue(gone == List(2L, 3L, 4L)) &&
      // оранжевый меч Ришелье вовсе не возьмёт: потолок — зелёный
      assertTrue(DailyRates.GuardRarity == Rarity.Green) &&
      assertTrue(!DailyKind.GuardWeapons.accepts(sword, None)) &&
      assertTrue(!DailyKind.GuardWeapons.accepts(
        thing(7L, "Сабля", ItemType.Weapon, ItemDetails.Plain).copy(rarity = Rarity.Blue), None)) &&
      assertTrue(DailyKind.GuardWeapons.accepts(bag(3), None)) &&
      // доспех для стражи — только нагрудник, и тоже не дороже зелёного
      assertTrue(!DailyKind.GuardWeapons.accepts(bag.last, None)) &&
      assertTrue(!DailyKind.GuardArmor.accepts(bag.last, None)) &&
      assertTrue(DailyKind.GuardArmor.accepts(
        thing(6L, "Кираса", ItemType.ChestPlate, ItemDetails.Plain).copy(rarity = Rarity.Green), None)) &&
      assertTrue(!DailyKind.GuardArmor.accepts(
        thing(8L, "Кираса", ItemType.ChestPlate, ItemDetails.Plain).copy(rarity = Rarity.Purple), None))
    },

    test("платят по делу: за лот меньше, за принесённое больше, и уровень тут ни при чём") {
      assertTrue(DailyRates.doubloons(DailyKind.BankLot) == 2L) &&
      assertTrue(DailyRates.doubloons(DailyKind.BankRelic) == 5L) &&
      assertTrue(DailyRates.doubloons(DailyKind.BankGem) == 5L) &&
      // редкая трава оплачивается редкой склянкой, прочие заказы — простой
      assertTrue((0L until 20L).forall(s => DailyRates.plainBrews.contains(DailyRates.brew(DailyKind.HerbsAny, s)))) &&
      assertTrue((0L until 20L).forall(s => DailyRates.rareBrews.contains(DailyRates.brew(DailyKind.HerbRare, s)))) &&
      assertTrue(DailyRates.rareBrews.forall(b => b.recipe.exists(_.herbRank >= 2))) &&
      assertTrue(DailyRates.plainBrews.forall(b => b.recipe.forall(_.herbRank <= 1)))
    },

    test("запись переживает jsonb, а пустая читается как «поручений ещё не было»") {
      val task  = DailyTask(DailyKind.BankGem, 123L, count = 1L,
                    pick = Some(GemKind.Ruby.entryName), taken = true)
      val all   = DailyQuests.empty.updated(DailyNpc.Rakhadim, task)
      val back  = all.asJson.as[DailyQuests]
      val fresh = io.circe.Json.obj().as[DailyQuests]
      assertTrue(back.contains(all)) &&
      assertTrue(fresh.contains(DailyQuests.empty)) &&
      assertTrue(all.today(DailyNpc.Rakhadim, 123L).nonEmpty) &&
      assertTrue(all.today(DailyNpc.Rakhadim, 124L).isEmpty)
    },

    test("круг целиком: взял у Ришелье, сделал, сдал — серебро, опыт и «на сегодня всё»") {
      import pangea.model.state.StateType
      import pangea.service.state.states.merchant.MerchantState
      import pangea.test._
      def tap(k: String) = UserAction("", Some(s"""{"action":"$k"}"""))
      for {
        d    <- dao(hero().copy(silver = 0L))
        r    <- TestRenderer.make
        c    <- ZIO.attempt(SceneContent.load())
        st    = MerchantState(d, TestInventoryRepository.accepting, TestItemRepository.make, c)
        h    <- d.getHeroByUserId(userId).map(_.get)
        // поручение выдаём сами: в пуле их пять, а проверяем механику сдачи
        _    <- seed(d, DailyNpc.Richelieu, DailyTask(DailyKind.SellItems, DailyRates.dayOf(0L)))
        _    <- st.action(testUser, tap("RichelieuDailyTake"), r)
        _    <- DailyQuestLog.add(d, userId, DailyNpc.Richelieu, DailyKind.SellItems, DailyKind.SellItems.goal)
        out  <- st.action(testUser, tap("RichelieuDailyHand"), r)
        said <- r.sentScreens.map(_.map(_.text).mkString("\n"))
        after <- d.getHeroByUserId(userId).map(_.get)
        log  <- DailyQuestLog.load(d, userId).map(_.of(DailyNpc.Richelieu).get)
      } yield assertTrue(out == StateType.Merchant && log.done) &&
              assertTrue(after.silver == DailyRates.silver(h.lvl) && after.exp >= DailyRates.exp(h.lvl)) &&
              assertTrue(said.contains("сверх уговора"))
    },

    test("круг с «принеси» у Ришелье: взял, отдал кнопкой, получил") {
      import pangea.model.state.StateType
      import pangea.service.state.states.merchant.MerchantState
      import pangea.test._
      def tap(k: String) = UserAction("", Some(s"""{"action":"$k"}"""))
      val bag = (1L to 3L).toList.map(i =>
        thing(i, s"Нож $i", ItemType.Weapon, ItemDetails.Plain))
      for {
        d     <- dao(hero().copy(silver = 0L))
        r     <- TestRenderer.make
        c     <- ZIO.attempt(SceneContent.load())
        inv    = TestInventoryRepository.withItems(bag)
        st     = MerchantState(d, inv, TestItemRepository.make, c)
        h     <- d.getHeroByUserId(userId).map(_.get)
        _     <- seed(d, DailyNpc.Richelieu, DailyTask(DailyKind.GuardWeapons, DailyRates.dayOf(0L)))
        _     <- st.action(testUser, tap("RichelieuDailyTake"), r)
        _     <- st.action(testUser, tap("RichelieuDailyGive"), r)
        given <- DailyQuestLog.load(d, userId).map(_.of(DailyNpc.Richelieu).get)
        out   <- st.action(testUser, tap("RichelieuDailyHand"), r)
        after <- d.getHeroByUserId(userId).map(_.get)
        log   <- DailyQuestLog.load(d, userId).map(_.of(DailyNpc.Richelieu).get)
      } yield assertTrue(given.count == 3L && given.ready && inv.snapshot.isEmpty) &&
              assertTrue(out == StateType.Merchant && log.done) &&
              assertTrue(after.silver == DailyRates.silver(h.lvl) && after.exp >= DailyRates.exp(h.lvl))
    },

    test("у всех поручений есть тексты, а у «принеси» с уточнением — и само уточнение") {
      for {
        c <- ZIO.attempt(SceneContent.load())
      } yield assertTrue(DailyKind.values.forall { k =>
        val base = s"daily.${k.npc.key}.tasks.${k.key}"
        List("offer", "active", "ready").forall(f => c.text(s"$base.$f").nonEmpty)
      }) &&
      assertTrue(DailyNpc.values.forall { n =>
        List("offerLabel", "activeLabel", "readyLabel", "take", "hand", "untilNext", "doneToday", "reward")
          .forall(f => c.text(s"daily.${n.key}.$f").nonEmpty)
      }) &&
      // у кого есть «принеси» — есть и слова про сдачу товара
      assertTrue(DailyNpc.values.filter(n => DailyKind.of(n).exists {
        case _: DailyBring => true; case _ => false
      }).forall { n =>
        List("give", "given", "nothingToGive").forall(f => c.text(s"daily.${n.key}.$f").nonEmpty)
      }) &&
      // где горожанин называет вещь поимённо, там имя и подставляется
      assertTrue(DailyKind.values.filter { case b: DailyBring => b.picks.nonEmpty; case _ => false }
        .forall(k => c.text(s"daily.${k.npc.key}.tasks.${k.key}.offer").contains("{what}")))
    }
  )
}
