package com.moonshot.model

import indigo._
import indigoextras.subsystems._
import indigoextras.subsystems.AutomataEvent
import com.moonshot.core.Prefabs

object Fumes {

  final case class FumePayload(val angle: Radians) extends AutomatonPayload

  val poolKey: AutomataPoolKey =
    AutomataPoolKey("fumes")

  def spawn(at: Point, lifeSpan: Seconds, angle: Radians): AutomataEvent =
    AutomataEvent.Spawn(poolKey, at, Some(lifeSpan), Some(FumePayload(angle)))

  def automaton(ejectionDistance: Int): Automaton =
    Automaton(
      AutomatonNode.OneOf(Prefabs.fumes),
      Seconds.zero
    ).withModifier(modifier(ejectionDistance))

  def subSystem(ejectionDistance: Int): Automata =
    Automata(
      poolKey = poolKey,
      automaton = automaton(ejectionDistance),
      layer = Automata.Layer.Game
    ).withMaxPoolSize(300)

  def modifier(ejectionDistance: Int)(seed: AutomatonSeedValues, node: SceneGraphNode): Signal[AutomatonUpdate] =
    (node, seed.payload) match {
      case (fumeParticle: Graphic, Some(FumePayload(angle))) =>
        val position =
          Signal.Lerp(
            seed.spawnedAt,
            seed.spawnedAt + Point((Math.sin(angle.value) * ejectionDistance).toInt, (Math.cos(angle.value) * ejectionDistance).toInt),
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
