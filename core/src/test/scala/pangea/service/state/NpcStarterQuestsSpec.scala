package pangea.service.state

import pangea.engine.SceneContent
import pangea.generator.item.MaterialGenerator
import pangea.model.bank.BankVault
import pangea.model.hero.Hero
import pangea.model.item.{Item, MaterialKind}
import pangea.model.quest.NpcQuest
import pangea.model.rune.{RuneStone, RuneStoneSize}
import pangea.model.user.{TelegramId, User, UserId, VkId}
import pangea.service.parcel.Parcels
import pangea.service.state.states.artifact.FetShopState
import pangea.service.state.states.bank.TradeHouseState
import pangea.service.state.states.guild.MentorKazimirState
import pangea.test._
import zio.ZIO
import zio.test._

/** Задания троих, к кому новичок доходит не сразу: Казимир просит целую
  * рунную плиту, Фет — кожу с Гнилого Джо, Рахадим — кусок с элементаля.
  * У всех один круг: взял → пришёл с пустыми руками → принёс → получил. */
object NpcStarterQuestsSpec extends ZIOSpecDefault {

  private val userId   = UserId(1L)
  private val testUser = User(userId, VkId("vk_test"), TelegramId("tg_test"))

  private def tap(key: String): UserAction = UserAction("", Some(s"""{"action":"$key"}"""))

  private def material(id: Long, kind: MaterialKind): Item = MaterialGenerator.item(kind).copy(id = id)

  private def plate(id: Long): Item = RuneStone.item(RuneStone.all.head, RuneStoneSize.Big).copy(id = id)

  private def shard(id: Long): Item = RuneStone.item(RuneStone.all.head, RuneStoneSize.Small).copy(id = id)

  private def hero: Hero = TestFixtures.hero(userId).copy(silver = 0L, doubloons = 0L, guildReputation = 0L)

  /** Один и тот же круг у всех троих: экран задания, отказ с пустыми руками,
    * сдача с вещью в сумке. Сцена и проверка награды — снаружи. */
  private def circle(
    state:   State,
    questId: String,
    heroDao: TestHeroDao,
    inv:     TestInventoryRepository,
    item:    Item
  ) =
    for {
      r      <- TestRenderer.make
      _      <- state.enter(testUser, r)
      menu   <- r.sentScreens.map(_.last)
      _      <- state.action(testUser, tap(questId), r)
      intro  <- r.sentScreens.map(_.last)
      _      <- state.action(testUser, tap(s"${questId}Accept"), r)
      // пришёл с пустыми руками — отказ, и задание всё ещё идёт
      _      <- state.action(testUser, tap(questId), r)
      empty  <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      halfway <- NpcQuestLog.load(heroDao, userId)
      _      <- inv.addItem(TestFixtures.hero(userId).id, item).ignore
      _      <- state.action(testUser, tap(questId), r)
      said   <- r.sentScreens.map(_.map(_.text).mkString("\n"))
      after  <- NpcQuestLog.load(heroDao, userId)
      _      <- state.enter(testUser, r)
      closed <- r.sentScreens.map(_.last)
    } yield (menu, intro, empty, halfway, said, after, closed, inv.snapshot)

  override def spec = suite("Задания Казимира, Фета и Рахадима")(

    test("Казимир берёт только целую плиту и платит именем в гильдии") {
      for {
        heroDao <- TestHeroDao.withHero(userId, hero)
        content <- ZIO.attempt(SceneContent.load())
        // в сумке осколок: его Казимир не возьмёт
        inv      = TestInventoryRepository.withItems(List(shard(1L)))
        state    = MentorKazimirState(heroDao, inv, content)
        t       <- circle(state, "KazQuest", heroDao, inv, plate(2L))
        (menu, intro, empty, halfway, said, after, closed, left) = t
        h       <- heroDao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(menu.choices.map(_.id).contains("KazQuest")) &&
              assertTrue(intro.text.contains("большой рунный камень") &&
                         intro.choices.map(_.id) == List("KazQuestAccept", "KazQuestDecline")) &&
              assertTrue(empty.contains("Это не плита") && halfway.isTaken(NpcQuest.Kazimir)) &&
              assertTrue(after.isDone(NpcQuest.Kazimir) && said.contains("Казимир прячет плиту")) &&
              // плита ушла, осколок остался при герое
              assertTrue(left.map(_.id) == List(1L)) &&
              assertTrue(h.guildReputation == MentorKazimirState.QuestReputation) &&
              // выполненное задание кнопки больше не показывает
              assertTrue(!closed.choices.map(_.id).contains("KazQuest"))
    },

    test("Фет берёт кожу упыря и отсыпает золотом") {
      for {
        heroDao <- TestHeroDao.withHero(userId, hero)
        content <- ZIO.attempt(SceneContent.load())
        // мифрил рядом лежит, но Фету он не нужен
        inv      = TestInventoryRepository.withItems(List(material(1L, MaterialKind.Mithril)))
        state    = FetShopState(heroDao, TestArtifactRepository.empty, inv, content)
        t       <- circle(state, "FetQuest", heroDao, inv,
                     material(2L, MaterialKind.GhoulSkin))
        (menu, intro, empty, halfway, said, after, closed, left) = t
        h       <- heroDao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(menu.choices.map(_.id).contains("FetQuest")) &&
              assertTrue(intro.text.contains("Гнилой Джо")) &&
              assertTrue(empty.contains("Это не она") && halfway.isTaken(NpcQuest.Fet)) &&
              assertTrue(after.isDone(NpcQuest.Fet) && said.contains("Фет отсчитывает")) &&
              assertTrue(left.map(_.id) == List(1L)) &&
              assertTrue(h.doubloons == FetShopState.QuestDoubloons) &&
              assertTrue(!closed.choices.map(_.id).contains("FetQuest"))
    },

    test("Рахадим берёт кусок с любого элементаля и даёт половину ячейки") {
      for {
        heroDao  <- TestHeroDao.withHero(userId, hero)
        content  <- ZIO.attempt(SceneContent.load())
        bankRepo  = TestBankRepository.of(0)
        parcels   = Parcels(TestParcelDao.empty, bankRepo, content)
        inv       = TestInventoryRepository.withItems(Nil)
        state     = TradeHouseState(heroDao, bankRepo, parcels, inv, content)
        t        <- circle(state, "RakhQuest", heroDao, inv,
                      material(2L, MaterialKind.MagicStone))
        (menu, intro, empty, halfway, said, after, closed, left) = t
        h        <- heroDao.getHeroByUserId(userId).map(_.get)
      } yield assertTrue(menu.choices.map(_.id).contains("RakhQuest")) &&
              // банкир учит ячейке, а не торгам: про аукцион в завязке ни слова
              assertTrue(intro.text.contains("залог") && !intro.text.toLowerCase.contains("аукцион")) &&
              assertTrue(empty.contains("Это не то") && halfway.isTaken(NpcQuest.Rakhadim)) &&
              assertTrue(after.isDone(NpcQuest.Rakhadim) && said.contains("Рахадим пододвигает")) &&
              assertTrue(left.isEmpty) &&
              assertTrue(h.silver == BankVault.FirstCellPrice / 2L) &&
              assertTrue(!closed.choices.map(_.id).contains("RakhQuest"))
    },

    test("железо огненного годится Рахадиму наравне с камнем, а чужое — нет") {
      assertTrue(TradeHouseState.elementalPiece(material(1L, MaterialKind.EverburningIron))) &&
      assertTrue(TradeHouseState.elementalPiece(material(2L, MaterialKind.MagicStone))) &&
      assertTrue(!TradeHouseState.elementalPiece(material(3L, MaterialKind.GhoulSkin))) &&
      assertTrue(FetShopState.ghoulSkin(material(4L, MaterialKind.GhoulSkin))) &&
      assertTrue(!FetShopState.ghoulSkin(material(5L, MaterialKind.WhiteWolfHide))) &&
      assertTrue(MentorKazimirState.bigRune(plate(6L)) && !MentorKazimirState.bigRune(shard(7L)))
    },

    test("у всех трёх заданий есть тексты, и ключи не разошлись с кодом") {
      for {
        c <- ZIO.attempt(SceneContent.load())
      } yield assertTrue(List(NpcQuest.Kazimir, NpcQuest.Fet, NpcQuest.Rakhadim).forall { q =>
        List("offerLabel", "activeLabel", "intro", "accept", "decline", "accepted",
             "step1Fail", "outro", "reward").forall(f => c.text(s"npcQuests.${q.key}.$f").nonEmpty)
      })
    }
  )
}
