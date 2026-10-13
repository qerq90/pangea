package pangea.service.referral

import pangea.engine.SceneContent
import pangea.model.hero.{AzatState, Hero, HeroId}
import pangea.model.referral.FriendCode
import pangea.model.state.StateType
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.admin.AdminPanel
import pangea.service.parcel.{Parcels, Transfers}
import pangea.service.payout.Payouts
import pangea.service.state.states.hero.{FriendCodeState, HeroStatsState}
import pangea.service.state.{PlayerLock, State, StateHandler, UserAction}
import pangea.test._
import zio.ZIO
import zio.test._

/** Код друга: сам код, экран в «Персонаже», ввод чужого кода и награды. */
object FriendCodeSpec extends ZIOSpecDefault {

  private val me     = User(UserId(1L), VkId("101"), TelegramId("tg1"))
  private val friend = User(UserId(2L), VkId("102"), TelegramId("tg2"))
  private val myHero     = HeroId(1L)
  private val friendHero = HeroId(2L)

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))
  private def typed(text: String): UserAction = UserAction(text, None)

  /** Я — герой `myLvl`, друг — двадцатого уровня. */
  private def world(myLvl: Long = 3L) =
    for {
      heroDao  <- TestHeroDao.withHero(me.userId,
                    TestFixtures.hero(me.userId, state = StateType.FriendCode).copy(id = myHero, lvl = myLvl))
      _        <- heroDao.insertHero(TestFixtures.hero(friend.userId).copy(id = friendHero, lvl = 20L))
      userRepo <- TestUserRepository.withUsers(me, friend)
      content  <- ZIO.attempt(SceneContent.load())
      renderer <- TestRenderer.make
      payoutDao = TestPayoutDao.empty
      payouts   = Payouts(payoutDao, heroDao, content)
      players   = new TestPlayers
      dao       = new TestReferralDao
      referrals = Referrals(dao, heroDao, userRepo, payouts, players, content)
    } yield (FriendCodeState(heroDao, referrals, content), referrals, heroDao, dao, payoutDao, players, renderer, content)

  private def azatOf(dao: TestHeroDao, user: User) =
    dao.readAzatData(user.userId).map(_.flatMap(_.as[AzatState].toOption).getOrElse(AzatState.empty))

  private def texts(r: TestRenderer) = r.sentScreens.map(_.map(_.text).mkString("\n"))

  private val day = 24L * 60L * 60L * 1000L

  override def spec = suite("Код друга")(

    test("код — восемь цифр, у соседних аккаунтов непохожий, читается обратно с любыми разделителями") {
      val ids   = List(1L, 2L, 3L, 42L, 9999L, 12345678L).map(UserId(_))
      val codes = ids.map(FriendCode.of)
      assertTrue(codes.forall(_.matches("\\d{4}-\\d{4}"))) &&
      assertTrue(ids.zip(codes).forall { case (id, code) => FriendCode.parse(code).contains(id) }) &&
      assertTrue(codes.distinct.size == codes.size) &&
      // у первого и второго коды не идут подряд
      assertTrue(codes.take(2).map(_.filter(_.isDigit).toLong) match {
        case a :: b :: Nil => math.abs(a - b) > 1000L
        case _             => false
      }) &&
      assertTrue(FriendCode.parse(" " + codes.head.replace("-", " ") + " ").contains(UserId(1L))) &&
      assertTrue(FriendCode.parse("123").isEmpty && FriendCode.parse("привет").isEmpty)
    },

    test("в «Персонаже» есть кнопка «Код друга»") {
      for {
        heroDao  <- TestHeroDao.withHero(me.userId, TestFixtures.hero(me.userId, state = StateType.HeroStats))
        content  <- ZIO.attempt(SceneContent.load())
        renderer <- TestRenderer.make
        state     = HeroStatsState(heroDao, content)
        _        <- state.enter(me, renderer)
        screen   <- renderer.sentScreens.map(_.last)
        next     <- state.action(me, tap("OpenFriendCode"), renderer)
      } yield assertTrue(screen.choices.exists(c => c.id == "OpenFriendCode" && c.label.contains("Код друга"))) &&
              assertTrue(next == StateType.FriendCode)
    },

    test("экран: свой код, а до 5 уровня — подсказка, куда вписать чужой") {
      for {
        t <- world(myLvl = 5L)
        (state, _, _, _, _, _, r, _) = t
        _      <- state.enter(me, r)
        young  <- r.sentScreens.map(_.last)
        u <- world(myLvl = 6L)
        (state6, _, _, _, _, _, r6, _) = u
        _      <- state6.enter(me, r6)
        grown  <- r6.sentScreens.map(_.last)
      } yield assertTrue(young.text.contains(FriendCode.of(me.userId)) && young.text.contains("напишите его сюда")) &&
              assertTrue(grown.text.contains(FriendCode.of(me.userId)) && !grown.text.contains("напишите его сюда")) &&
              assertTrue(young.choices.map(_.id) == List("Back"))
    },

    test("чужой код принят: мне — благословение на три дня, другу — три дублона и письмо") {
      for {
        t <- world()
        (state, _, heroDao, dao, payoutDao, players, r, _) = t
        _     <- TestClock.setTime(java.time.Instant.ofEpochMilli(10L * day))
        _     <- state.action(me, typed(FriendCode.of(friend.userId)), r)
        azat  <- azatOf(heroDao, me)
        said  <- texts(r)
        again <- state.action(me, typed(FriendCode.of(friend.userId)), r) *> texts(r)
        last  <- r.sentScreens.map(_.last)
      } yield assertTrue(dao.snapshot.get(me.userId).contains((friend.userId, false))) &&
              assertTrue(azat.blessingUntil.contains(10L * day + Referrals.BlessingMs)) &&
              // суточные отдыхи считаются с этой минуты
              assertTrue(azat.restsGrantedAt.contains(10L * day)) &&
              assertTrue(payoutDao.snapshot.get(friendHero).contains((0L, Referrals.OwnerNow))) &&
              assertTrue(players.sentLetters.exists { case (to, text) => to == friend.userId && text.contains("🟡 3") }) &&
              assertTrue(said.contains("Код принят")) &&
              // второй раз — нельзя, и подсказки больше нет
              assertTrue(again.contains("уже вписывали") && !last.text.contains("напишите его сюда"))
    },

    test("свой, несуществующий, не код, не тот уровень, обмен кодами — отказ и никаких наград") {
      for {
        t <- world()
        (state, referrals, heroDao, dao, payoutDao, players, r, _) = t
        _    <- state.action(me, typed(FriendCode.of(me.userId)), r)
        _    <- state.action(me, typed(FriendCode.of(UserId(777L))), r)
        _    <- state.action(me, typed("дай денег"), r)
        said <- texts(r)
        // друг сам вписал мой код — свой мне он отдать не может
        _    <- dao.add(friend.userId, me.userId, 0L)
        _    <- state.action(me, typed(FriendCode.of(friend.userId)), r)
        swap <- texts(r)
        u <- world(myLvl = 6L)
        (state6, _, _, _, payout6, _, r6, _) = u
        _    <- state6.action(me, typed(FriendCode.of(friend.userId)), r6)
        tall <- texts(r6)
        azat <- azatOf(heroDao, me)
      } yield assertTrue(said.contains("Свой код вписать нельзя")) &&
              assertTrue(said.contains("Такого кода нет")) &&
              assertTrue(said.contains("восемь цифр")) &&
              assertTrue(swap.contains("обменяться кодами нельзя")) &&
              assertTrue(tall.contains("не выше 5 уровня")) &&
              assertTrue(dao.snapshot.get(me.userId).isEmpty) &&
              assertTrue(payoutDao.snapshot.isEmpty && payout6.snapshot.isEmpty) &&
              assertTrue(players.sentLetters.isEmpty && azat.blessingUntil.isEmpty)
    },

    test("друг дорос до 10 уровня — владельцу кода ещё пять дублонов, ровно раз") {
      for {
        t <- world()
        (_, referrals, _, dao, payoutDao, players, _, _) = t
        _      <- dao.add(me.userId, friend.userId, 0L)
        at9    <- referrals.payLevelBonus(me, hero(9L)) *> ZIO.succeed(payoutDao.snapshot)
        _      <- referrals.payLevelBonus(me, hero(10L))
        _      <- referrals.payLevelBonus(me, hero(11L))
      } yield assertTrue(at9.isEmpty) &&
              assertTrue(payoutDao.snapshot.get(friendHero).contains((0L, Referrals.LevelBonus))) &&
              assertTrue(dao.snapshot.get(me.userId).contains((friend.userId, true))) &&
              assertTrue(players.sentLetters.count(_._1 == friend.userId) == 1)
    },

    test("бонус за 10 уровень приходит с ближайшим действием героя, а дублоны — где угодно, даже в бою") {
      for {
        content  <- ZIO.attempt(SceneContent.load())
        heroDao  <- TestHeroDao.withHero(me.userId,
                      TestFixtures.hero(me.userId, state = StateType.Battle).copy(id = myHero, lvl = 10L))
        _        <- heroDao.insertHero(TestFixtures.hero(friend.userId, state = StateType.Battle)
                      .copy(id = friendHero, lvl = 20L, doubloons = 4L, silver = 50L))
        userRepo <- TestUserRepository.withUsers(me, friend)
        api      <- TestApi.make
        lock     <- PlayerLock.make
        payoutDao = TestPayoutDao.empty
        payouts   = Payouts(payoutDao, heroDao, content)
        players   = new TestPlayers
        dao       = new TestReferralDao
        _        <- dao.add(me.userId, friend.userId, 0L)
        referrals = Referrals(dao, heroDao, userRepo, payouts, players, content)
        parcels   = Parcels(TestParcelDao.empty, TestBankRepository.empty, content)
        transfers = Transfers(TestInventoryRepository.accepting, userRepo, parcels, players, content,
                      heroDao, TestBankRepository.empty, payouts)
        handler   = new StateHandler(api, userRepo, new DaoBackedTestHeroRepository(heroDao), heroDao, payouts,
                      parcels, transfers, Map.empty[StateType, State], AdminPanel.disabled, content, lock,
                      None, Some(referrals))
        // моё действие: бонус уходит другу в отложенные выплаты
        _        <- handler.makeActionVK(me.vkId, 1L, UserAction("", None)).ignore
        queued    = payoutDao.snapshot.get(friendHero)
        // ещё и серебро ему кто-то оставил — его в бою не выдают
        _        <- payoutDao.add(friendHero, 300L, 0L)
        _        <- handler.makeActionVK(friend.vkId, 2L, UserAction("", None)).ignore
        his      <- heroDao.getHeroByUserId(friend.userId).map(_.get)
        sent     <- api.sentMessages.map(_.map(_._1).mkString("\n"))
      } yield assertTrue(queued.contains((0L, Referrals.LevelBonus))) &&
              // дублоны дошли в бою, серебро ждёт города
              assertTrue(his.doubloons == 4L + Referrals.LevelBonus && his.silver == 50L) &&
              assertTrue(payoutDao.snapshot.get(friendHero).contains((300L, 0L))) &&
              assertTrue(sent.contains("Вам пришли дублоны: 🟡 5"))
    },

    test("благословение без отсчёта отдыхов начинает отсчёт, а продление его не сбивает") {
      val noon = day / 2
      // так выглядело купленное в храме: срок есть, отсчёта нет — и отдыхи не капали
      val stuck   = AzatState(blessingUntil = Some(noon + 5 * day))
      val started = stuck.withDailyRests(noon)
      val nextDay = started.withDailyRests(noon + day)
      val fresh   = AzatState.empty.blessedFor(3 * day, noon)
      val ongoing = AzatState(blessingUntil = Some(noon + day), restsGrantedAt = Some(noon - day / 4))
        .blessedFor(3 * day, noon)
      assertTrue(started.restsGrantedAt.contains(noon) && started.instantRests == 0) &&
      assertTrue(nextDay.instantRests == AzatState.BlessingDailyRests) &&
      assertTrue(fresh.blessingUntil.contains(noon + 3 * day) && fresh.restsGrantedAt.contains(noon)) &&
      // продление — от остатка, отметка суток прежняя
      assertTrue(ongoing.blessingUntil.contains(noon + 4 * day) && ongoing.restsGrantedAt.contains(noon - day / 4))
    }
  )

  private def hero(lvl: Long): Hero = TestFixtures.hero(me.userId).copy(id = myHero, lvl = lvl)

}
