package com.papauschek.ui

import com.papauschek.puzzle.{Point, Puzzle, AnnotatedPoint}
import org.scalajs.dom

object HtmlRenderer:

  /** @return HTML for rendering a puzzle
   * @param puzzle the puzzle to render
   * @param showSolution if true, shows the solution (all characters) of the puzzle
   * @param showPartialSolution if true, shows partial solution based on selected mode
   * @param partialMode the mode for partial solution: "random", "odd", "even", or "custom"
   * @param customWordNumbers comma-separated word numbers for custom mode
   * @param numberColor the color for the word numbers (hex color code)
   * @param letterCase the case for letters: "uppercase" or "lowercase" */
  def renderPuzzle(puzzle: Puzzle,
                   showSolution: Boolean = false,
                   showPartialSolution: Boolean = false,
                   partialMode: String = "random",
                   customWordNumbers: String = "",
                   numberColor: String = "#999999",
                   letterCase: String = "uppercase"): String =

    val annotation = puzzle.getAnnotation

    /** Helper method to collect all points from words with specified numbers */
    def collectAllPointsFromWords(puzzle: Puzzle, annotation: Map[Point, Seq[AnnotatedPoint]], wordNumbers: Set[Int]): Set[Point] =
      val allPoints = collection.mutable.Set.empty[Point]
      
      // For each annotation point, check if it belongs to a word with the specified number
      annotation.foreach { case (startPoint, annotatedPoints) =>
        annotatedPoints.foreach { annotatedPoint =>
          if (wordNumbers.contains(annotatedPoint.index)) {
            // Collect all points for this word
            val word = annotatedPoint.word
            val vertical = annotatedPoint.vertical
            
            // Add all points of this word
            (0 until word.length).foreach { index =>
              val point = if (vertical) {
                Point(startPoint.x, startPoint.y + index)
              } else {
                Point(startPoint.x + index, startPoint.y)
              }
              allPoints += point
            }
          }
        }
      }
      
      allPoints.toSet

    // Determine which points should be shown based on the mode
    val visiblePoints = if (showPartialSolution) {
      partialMode match {
        case "random" => 
          // Use existing random logic
          puzzle.getCharsShownInPartialSolution()
        case "odd" =>
          // Collect all points from odd-numbered words
          val oddWordNumbers = annotation.values.flatten.map(_.index).filter(_ % 2 == 1).toSet
          collectAllPointsFromWords(puzzle, annotation, oddWordNumbers)
        case "even" =>
          // Collect all points from even-numbered words
          val evenWordNumbers = annotation.values.flatten.map(_.index).filter(_ % 2 == 0).toSet
          collectAllPointsFromWords(puzzle, annotation, evenWordNumbers)
        case "custom" =>
          // Collect all points from custom word numbers
          val customWordNums = customWordNumbers.split(",").map(_.trim).filter(_.nonEmpty).flatMap(s => 
            try Some(s.toInt)
            catch case _: NumberFormatException => None
          ).toSet
          collectAllPointsFromWords(puzzle, annotation, customWordNums)
        case _ => Set.empty[Point]
      }
    } else {
      Set.empty[Point]
    }

    def renderCell(x: Int, y: Int): String =
      puzzle.getChar(x, y) match {
        case ' ' => ""
        case char =>
          // Show letter if in full solution OR if cell is in visible points
          val showLetter = showSolution || {
            showPartialSolution && visiblePoints.contains(Point(x, y))
          }
          
          val displayChar = if (letterCase == "lowercase") char.toString.toLowerCase else char.toString.toUpperCase
          val yPos = if (letterCase == "lowercase") y * 10 + 5.5 else y * 10 + 6
          val svgLetter = Option.when(showLetter) {
            s"""<text x="${x * 10 + 5}" y="$yPos" text-anchor="middle" dominant-baseline="middle" class="letter">$displayChar</text>"""
          }

          val svgAnnotation = annotation.get(Point(x, y)) match {
            case Some(anno) if anno.nonEmpty =>
              val annotationIndices = anno.map(_.index).mkString(",")
              val horizontalAnnotations = anno.filter(!_.vertical)
              val verticalAnnotations = anno.filter(_.vertical)
              
              val horizontalText = if (horizontalAnnotations.nonEmpty) {
                val indices = horizontalAnnotations.map(_.index).mkString(",")
                s"""<text x="${x * 10 - 1.5}" y="${y * 10 + 5}" text-anchor="end" dominant-baseline="middle" class="annotation-horizontal">$indices</text>"""
              } else ""
              
              val verticalText = if (verticalAnnotations.nonEmpty) {
                val indices = verticalAnnotations.map(_.index).mkString(",")
                s"""<text x="${x * 10 + 5}" y="${y * 10 - 3}" text-anchor="middle" dominant-baseline="middle" class="annotation-vertical">$indices</text>"""
              } else ""
              
              Some(horizontalText + verticalText)
            case _ => None
          }

          val svgCell = s"""<rect x="${x * 10}" y="${y * 10}" rx="0.5" ry="0.5" width="10" height="10"
            |  style="fill:white;stroke:black;stroke-width:0.3" />""".stripMargin

          svgCell + svgAnnotation.mkString + svgLetter.mkString
      }

    def renderHeight(y: Int): String =
      (0 until puzzle.config.width).map(renderCell(_, y)).mkString("\r\n")

    val renderedPuzzle = (0 until puzzle.config.height).map(renderHeight).mkString("\r\n")

    s"""<svg viewBox="-8 -8 ${puzzle.config.width * 10 + 15} ${puzzle.config.height * 10 + 15}">
      |  <style>
      |    .annotation-horizontal {
      |      font: 5px sans-serif;
      |      fill: ${numberColor};
      |    }
      |    .annotation-vertical {
      |      font: 5px sans-serif;
      |      fill: ${numberColor};
      |    }
      |    .letter {
      |      font: 8px sans-serif;
      |      fill: black;
      |    }
      |  </style>
      |  $renderedPuzzle
      |</svg>""".stripMargin


  val alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

  /** @return HTML representing the clues (= solution) words for this puzzle */
  def renderClues(puzzle: Puzzle, extraWords: Set[String]): String =

    val annotations = puzzle.getFullAnnotation.sortBy(_.index)

    def renderDescriptions(vertical: Boolean): String = {
      val sortedAnnotationValues =
        if (vertical) annotations.sortBy(a => (a.location.x, a.location.y))
        else annotations.sortBy(a => (a.location.y, a.location.x))
      sortedAnnotationValues.filter(_.vertical == vertical).map {
        p =>
          val formattedWord = if (extraWords.contains(p.fullWord)) s"<strong>${p.fullWord}</strong>" else p.fullWord
          val formattedLocation = s"${p.location.x + 1}${alphabet.lift.apply(p.location.y).getOrElse(' ')}"
          "<div>" + p.index + ") " + formattedWord + "</div>"
          //"<div>" + formattedLocation + ") " + formattedWord + "</div>"
      }.mkString("\r\n")
    }

    s"""<div class="row">
       |  <div class="col-lg-6">
       |    <h4>Horizontal</h4>
       |    <p>${renderDescriptions(vertical = false)}</p>
       |  </div>
       |  <div class="col-lg-6">
       |    <h4>Vertical</h4>
       |    <p>${renderDescriptions(vertical = true)}</p>
       |  </div>
       |</div>
       |""".stripMargin


  /** @return HTML representing some additional info about the puzzle, such as density and discarded words. */
  def renderPuzzleInfo(puzzle: Puzzle, unusedWords: Seq[String]): String =
    val infoText = s"This puzzle has a <strong>density of ${(puzzle.density * 100).round}%</strong>. " +
      s"This is the area covered by letters. " +
      s"If you prefer a more dense puzzle, add more words to the list above and let the tool discard the words that do not fit well. "
    val unusedInfoText = Option.when(unusedWords.nonEmpty)(s"The following words from your list were NOT used: ${unusedWords.mkString(", ")}").mkString
    infoText + unusedInfoText

