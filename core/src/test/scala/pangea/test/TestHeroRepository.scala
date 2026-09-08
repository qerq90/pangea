package pangea.test

import pangea.model.hero.Hero
import pangea.model.state.StateType
import pangea.model.user.UserId
import pangea.repository.hero.HeroRepository
import zio.{Ref, Task, ZIO}

class TestHeroRepository(heroRef: Ref[Map[UserId, Hero]]) extends HeroRepository {

  def registerNewHero(userId: UserId): Task[Hero] =
    ZIO.fail(new Throwable("TestHeroRepository: registerNewHero not supported, seed a hero via TestHeroRepository.withHero"))

  def getHero(userId: UserId): Task[Option[Hero]] = heroRef.get.map(_.get(userId))

  def updateState(userId: UserId, potentiallyNewState: StateType): Task[Unit] =
    heroRef.update(m => m.get(userId).fold(m)(h => m.updated(userId, h.copy(state = potentiallyNewState))))

  def snapshot(userId: UserId): Task[Option[Hero]] = getHero(userId)
}

object TestHeroRepository {
  def withHero(userId: UserId, hero: Hero): Task[TestHeroRepository] =
    Ref.make(Map(userId -> hero)).map(new TestHeroRepository(_))
}
