package com.moonshot.model

import indigo._
import indigoextras.subsystems._
import com.moonshot.core.Assets
import indigoextras.subsystems.AutomataEvent

object Fumes {

  final case class FumePayload(val angle: Radians) extends AutomatonPayload

  val poolKey: AutomataPoolKey =
    AutomataPoolKey("fumes")

  def spawn(at: Point, lifeSpan: Seconds, angle: Radians): AutomataEvent =
    AutomataEvent.Spawn(poolKey, at, Some(lifeSpan), Some(FumePayload(angle)))

  val automaton: Automaton =
    Automaton(
      AutomatonNode.OneOf(Assets.Rocket.clouds),
      Seconds.zero
    ).withModifier(modifier)

  val subSystem: Automata =
    Automata(
      poolKey = poolKey,
      automaton = automaton,
      layer = Automata.Layer.Game
    ).withMaxPoolSize(300)

  def modifier(seed: AutomatonSeedValues, node: SceneGraphNode): Signal[AutomatonUpdate] =
    (node, seed.payload) match {
      case (fumeParticle: Graphic, Some(FumePayload(angle))) =>
        val position =
          Signal.Lerp(
            seed.spawnedAt,
            seed.spawnedAt + Point((Math.sin(angle.value) * 300).toInt, (Math.cos(angle.value) * 300).toInt),
            seed.lifeSpan
          )

        Signal.product(Signal.SinWave, position).map {
          case (amt, pt) =>
            val scaleFactor = Math.min(1.0, amt * 2)

            AutomatonUpdate(
              fumeParticle
                .moveTo(pt)
                .withAlpha(amt)
                .scaleBy(scaleFactor, scaleFactor)
            )
        }

      case _ =>
        Signal.fixed(AutomatonUpdate.empty)
    }

}
