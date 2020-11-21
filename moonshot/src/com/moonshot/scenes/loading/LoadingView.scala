package com.moonshot.scenes.loading

import indigo._
import com.moonshot.core.Assets.Font

object LoadingView {

  def draw(screenDimensions: Rectangle, loadingState: LoadingState): SceneUpdateFragment = {
    val x = screenDimensions.horizontalCenter
    val y = screenDimensions.verticalCenter

    val message: String =
      loadingState match {
        case LoadingState.NotStarted =>
          "Loading..."

        case LoadingState.InProgress(percent) =>
          s"Loading...${percent.toString()}%"

        case LoadingState.Complete =>
          "Loading...100%"

        case LoadingState.Error =>
          "Uh oh, loading failed..."
      }

    SceneUpdateFragment(
      Text(
        message,
        x,
        y + 10,
        1,
        Font.fontKey
      ).alignCenter
    )
  }

}
