package puyo

import scalafx.application.JFXApp

import scalafx.scene.Scene

object Main extends JFXApp {
  stage = new JFXApp.PrimaryStage{
    title = "Puyo Puyo"
    scene  = new Scene(6*30,12*30)
  }
}