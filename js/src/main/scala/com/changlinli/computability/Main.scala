package com.changlinli.computability

import scala.scalajs.js
import scala.scalajs.js.annotation.{JSExport, JSExportTopLevel}
import org.scalajs.dom
import org.scalajs.dom.html
import dom.document
import org.scalajs.dom.html.Paragraph
import scalatags.JsDom.all._

object Main extends js.JSApp {
  def appendPar(targetNode: dom.Node, text: String): Unit = {
    val parNode = document.createElement("p")
    val textNode = document.createTextNode(text)
    parNode.appendChild(textNode)
    targetNode.appendChild(parNode)
  }

  def createParagraph(text: String): Paragraph = {
    val parNode = document.createElement("p")
    val textNode = document.createTextNode(text)
    parNode.appendChild(textNode)
    parNode.asInstanceOf[html.Paragraph]
  }

  @JSExportTopLevel("lambdaHandler")
  def lambdaHandler(): Unit = {
    val inputValue = document.getElementById("code-entry").asInstanceOf[html.Input].value
    val outputElement = document.getElementById("result").asInstanceOf[html.Paragraph]
    val lambdaCalculation = ArithmeticOperation.calculateFromString(inputValue)
    val finalResult = lambdaCalculation
      .map(_.last)
      .flatMap(LambdaTerm.convertToNaturalNum)
      .map(NaturalNum.toInt)
      .getOrElse("Oh no there was an error!")
    val finalResultString = finalResult.toString
    val htmlString = lambdaCalculation
      .map(lambdaTerms => lambdaTerms.map(LambdaTerm.prettyPrint))
      .toList
      .flatMap(_.toList)
      .map(x => p(x))
      .::(p("Lambda Calculus Reduction Terms:"))
      .::(p(s"Final Result: $finalResultString"))
      .mkString("\n")
    outputElement.innerHTML = htmlString
  }

  @JSExportTopLevel("deBruijnHandler")
  def deBruijnHandler(): Unit = {
    val inputValue = document.getElementById("code-entry").asInstanceOf[html.Input].value
    val outputElement = document.getElementById("result").asInstanceOf[html.Paragraph]
    val lambdaCalculation = ArithmeticOperation.calculateDeBruijnFromString(inputValue)
    val finalResult = lambdaCalculation
      .map(_.last)
      .flatMap(DeBruijnLambdaTerm.convertToNaturalNum)
      .map(NaturalNum.toInt)
      .getOrElse("Oh no there was an error!")
    val finalResultString = finalResult.toString
    val result = lambdaCalculation
      .map(lambdaTerms => lambdaTerms.map(DeBruijnLambdaTerm.prettyPrint))
      .toList
      .flatMap(_.toList)
      .map(x => p(x))
      .::(p("DeBruijn-Indexed Lambda Calculus Reduction Terms:"))
      .::(p(s"Final Result: $finalResultString"))
      .mkString("\n")
    outputElement.innerHTML = result
  }

  @JSExport
  def main(): Unit = {
    println("Starting up...")
  }
}
