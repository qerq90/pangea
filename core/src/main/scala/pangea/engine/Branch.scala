package pangea.engine

import io.circe.jawn
import pangea.model.state.StateType
import pangea.model.user.User
import pangea.service.state.UserAction
import zio.{Task, ZIO}

class Branch(routes: Map[String, Target], fallback: Target) {

  def act(user: User, ua: UserAction, renderer: Renderer): Task[StateType] = {
    val key = ua.payload
      .flatMap(p => jawn.decode[Map[String, String]](p).toOption.flatMap(_.get("action")))
      .getOrElse("Text")
    dispatch(routes.getOrElse(key, fallback), user, ua, renderer)
  }

  def gotoTargets: Set[StateType] =
    (routes.values.toList :+ fallback).collect { case Target.Goto(st) => st }.toSet

  private def dispatch(target: Target, user: User, ua: UserAction, renderer: Renderer): Task[StateType] =
    target match {
      case Target.Goto(st) => ZIO.succeed(st)
      case Target.Run(f)   => f(user, ua, renderer)
    }
}
