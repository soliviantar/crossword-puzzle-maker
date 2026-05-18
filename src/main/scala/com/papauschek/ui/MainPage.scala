package com.papauschek.ui

import com.papauschek.puzzle.{Puzzle, PuzzleConfig, PuzzleWords}
import com.papauschek.ui.{Globals, HtmlRenderer}
import org.scalajs.dom
import org.scalajs.dom.Worker
import org.scalajs.dom.html.{Button, Div, Input, Select, TextArea}
import upickle.default.*
import concurrent.ExecutionContext.Implicits.global
import scala.scalajs.js.annotation.JSExport

/** the main user interface based on the `index.html` */
class MainPage:

  private var initialPuzzle: Puzzle = Puzzle.empty(PuzzleConfig())
  private var refinedPuzzle: Puzzle = initialPuzzle

  private var mainInputWords: Seq[String] = Nil
  private var wordClues: Map[String, String] = Map.empty

  private val showSolutionsCheckbox = dom.document.getElementById("show-clues-solutions").asInstanceOf[Input]
  private val showCluesCheckbox = dom.document.getElementById("show-clues-clues").asInstanceOf[Input]
  private val printSolutionsCheckbox = dom.document.getElementById("print-clues-solutions").asInstanceOf[Input]
  private val includeAllWordsCheckbox = dom.document.getElementById("include-all-words").asInstanceOf[Input]

  private val inputElement = dom.document.getElementById("input").asInstanceOf[TextArea]
  private val outputPuzzleElement = dom.document.getElementById("output-puzzle")
  private val outputCluesElement = dom.document.getElementById("output-clues")
  private val resultInfoElement = dom.document.getElementById("result-info")
  private val wordInclusionStatusElement = dom.document.getElementById("word-inclusion-status").asInstanceOf[org.scalajs.dom.html.Element]

  private val generateButton = dom.document.getElementById("generate-button").asInstanceOf[Button]
  private val generateSpinner = dom.document.getElementById("generate-spinner").asInstanceOf[Div]

  private val resultWithoutElement = dom.document.getElementById("result-without").asInstanceOf[Input]
  private val resultPartialElement = dom.document.getElementById("result-partial").asInstanceOf[Input]
  private val resultFullElement = dom.document.getElementById("result-full").asInstanceOf[Input]

  private val partialSubmenu = dom.document.getElementById("partial-submenu").asInstanceOf[org.scalajs.dom.html.Element]
  private val partialRandomElement = dom.document.getElementById("partial-random").asInstanceOf[Input]
  private val partialOddElement = dom.document.getElementById("partial-odd").asInstanceOf[Input]
  private val partialEvenElement = dom.document.getElementById("partial-even").asInstanceOf[Input]
  private val partialOddEvenElement = dom.document.getElementById("partial-oddeven").asInstanceOf[Input]
  private val partialCustomElement = dom.document.getElementById("partial-custom").asInstanceOf[Input]
  
  private val customInputContainer = dom.document.getElementById("custom-input-container").asInstanceOf[org.scalajs.dom.html.Element]
  private val customWordNumbersElement = dom.document.getElementById("custom-word-numbers").asInstanceOf[Input]

  private val commonGridOptionsContainer = dom.document.getElementById("common-grid-options-container").asInstanceOf[org.scalajs.dom.html.Element]
  private val oddevenOptionsContainer = dom.document.getElementById("oddeven-options-container").asInstanceOf[org.scalajs.dom.html.Element]
  private val gridTitle1Label = dom.document.getElementById("grid-title-1-label").asInstanceOf[org.scalajs.dom.html.Label]
  private val gridInstruction1Label = dom.document.getElementById("grid-instruction-1-label").asInstanceOf[org.scalajs.dom.html.Label]
  private val gridName1Element = dom.document.getElementById("grid-name-1").asInstanceOf[Input]
  private val gridName2Element = dom.document.getElementById("grid-name-2").asInstanceOf[Input]
  private val titleSizeElement = dom.document.getElementById("title-size").asInstanceOf[Input]
  private val gridInstruction1Element = dom.document.getElementById("grid-instruction-1").asInstanceOf[TextArea]
  private val gridInstruction2Element = dom.document.getElementById("grid-instruction-2").asInstanceOf[TextArea]
  private val instructionFontSizeElement = dom.document.getElementById("instruction-font-size").asInstanceOf[Input]
  
  private val btnBold1 = dom.document.getElementById("btn-bold-1").asInstanceOf[Button]
  private val btnItalic1 = dom.document.getElementById("btn-italic-1").asInstanceOf[Button]
  private val btnBold2 = dom.document.getElementById("btn-bold-2").asInstanceOf[Button]
  private val btnItalic2 = dom.document.getElementById("btn-italic-2").asInstanceOf[Button]

  private val numberLightGrayElement = dom.document.getElementById("number-light-gray").asInstanceOf[Input]
  private val numberDarkGrayElement = dom.document.getElementById("number-dark-gray").asInstanceOf[Input]
  private val numberBlackElement = dom.document.getElementById("number-black").asInstanceOf[Input]

  private val letterUppercaseElement = dom.document.getElementById("letter-uppercase").asInstanceOf[Input]
  private val letterLowercaseElement = dom.document.getElementById("letter-lowercase").asInstanceOf[Input]

  private val widthInputElement = dom.document.getElementById("width").asInstanceOf[Input]
  private val heightInputElement = dom.document.getElementById("height").asInstanceOf[Input]

  private val languageSelect = dom.document.getElementById("language-select").asInstanceOf[Select]
  private val refineButton = dom.document.getElementById("refine-button").asInstanceOf[Button]
  private val refreshButton = dom.document.getElementById("refresh-button").asInstanceOf[Button]
  private val refreshIcon = refreshButton.querySelector("i").asInstanceOf[org.scalajs.dom.html.Element]
  private val printButton = dom.document.getElementById("print-button").asInstanceOf[Button]

  private val resultRow = dom.document.getElementById("result-row").asInstanceOf[Div]
  private val refineRow = dom.document.getElementById("refine-row").asInstanceOf[Div]
  private val cluesRow = dom.document.getElementById("clues-row").asInstanceOf[Div]

  private val mainTitleElement = dom.document.getElementById("main-title").asInstanceOf[org.scalajs.dom.html.Heading]
  private val mainTitleInputElement = dom.document.getElementById("main-title-input").asInstanceOf[Input]
  
  private def updateMainTitle(): Unit = {
    val newTitle = mainTitleInputElement.value
    // Use a non-breaking space when empty so the h1 keeps its full height
    mainTitleElement.textContent = if (newTitle.isEmpty) "\u00a0" else newTitle
    mainTitleElement.style.display = "block"
  }

  mainTitleInputElement.addEventListener("input", { _ => 
    updateMainTitle()
    renderSolution()
  })
  mainTitleInputElement.addEventListener("change", { _ => 
    updateMainTitle()
    renderSolution()
  })
  updateMainTitle()

  generateButton.addEventListener("click", { _ => generateSolution() })
  refineButton.addEventListener("click", { _ => refineSolution() })
  refreshButton.addEventListener("click", { _ => generateSolution() })
  printButton.addEventListener("click", { _ => printSolution() })

  resultWithoutElement.addEventListener("click", { _ => 
    hidePartialSubmenu()
    hideCustomInput()
    hideOddevenOptions()
    renderSolution()
  })
  resultPartialElement.addEventListener("click", { _ => 
    showPartialSubmenu()
    renderSolution()
  })
  resultFullElement.addEventListener("click", { _ => 
    hidePartialSubmenu()
    hideCustomInput()
    hideOddevenOptions()
    renderSolution()
  })

  partialRandomElement.addEventListener("click", { _ =>
    hideCustomInput()
    hideOddevenOptions()
    renderSolution()
  })
  partialOddElement.addEventListener("click", { _ =>
    hideCustomInput()
    hideOddevenOptions()
    renderSolution()
  })
  partialEvenElement.addEventListener("click", { _ =>
    hideCustomInput()
    hideOddevenOptions()
    renderSolution()
  })
  partialOddEvenElement.addEventListener("click", { _ =>
    hideCustomInput()
    showOddevenOptions()
    renderSolution()
  })
  partialCustomElement.addEventListener("click", { _ => 
    showCustomInput()
    hideOddevenOptions()
    renderSolution()
  })
  
  customWordNumbersElement.addEventListener("input", { _ => renderSolution() })

  // Event listeners for oddeven options
  gridName1Element.addEventListener("input", { _ => renderSolution() })
  gridName2Element.addEventListener("input", { _ => renderSolution() })
  titleSizeElement.addEventListener("input", { _ => renderSolution() })
  gridInstruction1Element.addEventListener("input", { _ => renderSolution() })
  gridInstruction2Element.addEventListener("input", { _ => renderSolution() })
  instructionFontSizeElement.addEventListener("input", { _ => renderSolution() })

  btnBold1.addEventListener("click", { (e: dom.Event) =>
    e.preventDefault()
    applyTagToInstruction(gridInstruction1Element, "b")
  })
  btnItalic1.addEventListener("click", { (e: dom.Event) =>
    e.preventDefault()
    applyTagToInstruction(gridInstruction1Element, "i")
  })
  btnBold2.addEventListener("click", { (e: dom.Event) =>
    e.preventDefault()
    applyTagToInstruction(gridInstruction2Element, "b")
  })
  btnItalic2.addEventListener("click", { (e: dom.Event) =>
    e.preventDefault()
    applyTagToInstruction(gridInstruction2Element, "i")
  })

  // Initialize partial submenu visibility
  showPartialSubmenu()
  updatePrintVisibility()

  numberLightGrayElement.addEventListener("click", { _ => renderSolution() })
  numberDarkGrayElement.addEventListener("click", { _ => renderSolution() })
  numberBlackElement.addEventListener("click", { _ => renderSolution() })

  letterUppercaseElement.addEventListener("click", { _ => renderSolution() })
  letterLowercaseElement.addEventListener("click", { _ => renderSolution() })

  showSolutionsCheckbox.addEventListener("change", { _ => renderSolution() })
  showCluesCheckbox.addEventListener("change", { _ => renderSolution() })
  printSolutionsCheckbox.addEventListener("change", { _ => updatePrintVisibility() })

  /** read the words from the user interface and generate the puzzle in the background using web workers */
  def generateSolution(): Unit =
    val lines = inputElement.value.linesIterator.map(_.trim).filter(line => line.nonEmpty && !line.startsWith("#")).toSeq
    val parsedLines = lines.map { line =>
      val parts = line.split(":", 2)
      val rawWord = parts(0).trim
      val clue = if (parts.length > 1) parts(1).trim else ""
      (rawWord, clue)
    }
    wordClues = parsedLines.map { case (w, c) => (normalizeWord(w), c) }.toMap
    val inputWords = parsedLines.map(_._1).map(normalizeWord).filter(_.nonEmpty)

    if (inputWords.nonEmpty) {
      mainInputWords = PuzzleWords.sortByBest(inputWords)
      val startWidth = widthInputElement.valueAsNumber.toInt
      val height = heightInputElement.valueAsNumber.toInt

      generateSpinner.classList.remove("invisible")
      generateButton.classList.add("invisible")
      refreshIcon.classList.add("spinning")
      refreshButton.asInstanceOf[Input].disabled = true

      if (includeAllWordsCheckbox.checked) {
        runIncludeAllWords(startWidth, height, tryCount = 1)
      } else {
        val puzzleConfig = PuzzleConfig(width = startWidth, height = height)
        PuzzleGenerator.send(NewPuzzleMessage(puzzleConfig, mainInputWords)).foreach { puzzles =>
          finishGeneration(puzzles)
        }
      }
    }

  private def finishGeneration(puzzles: Seq[Puzzle]): Unit =
    generateSpinner.classList.add("invisible")
    generateButton.classList.remove("invisible")
    refreshIcon.classList.remove("spinning")
    refreshButton.asInstanceOf[Input].disabled = false
    resultRow.classList.remove("invisible")
    refineRow.classList.remove("invisible")
    cluesRow.classList.remove("invisible")
    initialPuzzle = puzzles.maxBy(_.density)
    refinedPuzzle = initialPuzzle
    renderSolution()
    updatePrintVisibility()

  private def runIncludeAllWords(width: Int, height: Int, tryCount: Int): Unit =
    widthInputElement.value = width.toString
    heightInputElement.value = height.toString
    val puzzleConfig = PuzzleConfig(width = width, height = height)
    PuzzleGenerator.send(NewPuzzleMessage(puzzleConfig, mainInputWords)).foreach { puzzles =>
      val bestPuzzle = puzzles.maxBy(_.density)
      val unusedWords = mainInputWords.filterNot(bestPuzzle.words.contains)

      if (unusedWords.isEmpty) {
        // Success! All words fit
        finishGeneration(puzzles)
      } else if (tryCount < 3) {
        // Retry with same size
        runIncludeAllWords(width, height, tryCount + 1)
      } else if (width < 36) {
        // Make grid width bigger by 1 and retry
        runIncludeAllWords(width + 1, height, tryCount = 1)
      } else if (height < 20) {
        // Width reached 36, make grid height bigger by 1 and retry
        runIncludeAllWords(width, height + 1, tryCount = 1)
      } else {
        // Reached maximum width 36 and maximum height 20, finish and display best puzzle found
        finishGeneration(puzzles)
      }
    }


  /** update print visibility of clues and solutions */
  def updatePrintVisibility(): Unit =
    if (printSolutionsCheckbox.checked) {
      cluesRow.classList.remove("print-hidden")
    } else {
      cluesRow.classList.add("print-hidden")
    }

  /** show partial solution submenu */
  def showPartialSubmenu(): Unit =
    partialSubmenu.style.display = "flex"

  /** hide partial solution submenu */
  def hidePartialSubmenu(): Unit =
    partialSubmenu.style.display = "none"

  /** show custom input field */
  def showCustomInput(): Unit =
    customInputContainer.style.display = "block"

  /** hide custom input field */
  def hideCustomInput(): Unit =
    customInputContainer.style.display = "none"

  /** show oddeven options container */
  def showOddevenOptions(): Unit =
    commonGridOptionsContainer.className = "col-12 col-lg-6"
    gridTitle1Label.textContent = "First Grid Title:"
    gridInstruction1Label.textContent = "First Grid Instructions:"
    gridInstruction1Element.placeholder = "Instructions for the first grid"
    if (gridName1Element.value.isEmpty) {
      gridName1Element.value = "A"
    }
    oddevenOptionsContainer.style.display = "block"

  /** hide oddeven options container */
  def hideOddevenOptions(): Unit =
    commonGridOptionsContainer.className = "col-12"
    gridTitle1Label.textContent = "Grid Title:"
    gridInstruction1Label.textContent = "Instructions:"
    gridInstruction1Element.placeholder = "Instructions for the grid"
    if (gridName1Element.value == "A") {
      gridName1Element.value = ""
    }
    oddevenOptionsContainer.style.display = "none"

  /** show the generated puzzle */
  def renderSolution(): Unit =
    val showPartialSolution = resultPartialElement.checked
    val showFullSolution = resultFullElement.checked
    
    val partialMode = 
      if (partialRandomElement.checked) "random"
      else if (partialOddElement.checked) "odd"
      else if (partialEvenElement.checked) "even"
      else if (partialOddEvenElement.checked) "oddeven"
      else if (partialCustomElement.checked) "custom"
      else "random"
    
    val customWordNumbers = customWordNumbersElement.value
    
    val numberColor = 
      if (numberLightGrayElement.checked) "#999999"
      else if (numberDarkGrayElement.checked) "#666666"
      else "#000000" // black

    val letterCase =
      if (letterUppercaseElement.checked) "uppercase"
      else "lowercase"

    val gridName1 = gridName1Element.value
    val gridName2 = gridName2Element.value
    val titleSize = titleSizeElement.value
    val gridInstruction1 = gridInstruction1Element.value
    val gridInstruction2 = gridInstruction2Element.value
    val instructionFontSize = instructionFontSizeElement.value

    outputPuzzleElement.innerHTML = HtmlRenderer.renderPuzzle(
      refinedPuzzle,
      showSolution = showFullSolution,
      showPartialSolution = showPartialSolution,
      partialMode = partialMode,
      customWordNumbers = customWordNumbers,
      numberColor = numberColor,
      letterCase = letterCase,
      gridName1 = gridName1,
      gridName2 = gridName2,
      titleSize = titleSize,
      gridInstruction1 = gridInstruction1,
      gridInstruction2 = gridInstruction2,
      instructionFontSize = instructionFontSize,
      mainTitle = mainTitleInputElement.value)

    val unusedWords = mainInputWords.filterNot(refinedPuzzle.words.contains)
    val extraWords = refinedPuzzle.words -- initialPuzzle.words
    resultInfoElement.innerHTML = HtmlRenderer.renderPuzzleInfo(refinedPuzzle, unusedWords)
    
    if (unusedWords.isEmpty) {
      wordInclusionStatusElement.innerHTML = "<strong>All your words were included.</strong>"
      wordInclusionStatusElement.setAttribute("style", "")
    } else {
      wordInclusionStatusElement.innerHTML = s"<strong>The following words were not used:</strong> ${unusedWords.mkString(", ")}"
      wordInclusionStatusElement.setAttribute("style", "color: red; font-size: 1.3rem;")
    }

    outputCluesElement.innerHTML = HtmlRenderer.renderClues(
      refinedPuzzle,
      extraWords,
      wordClues,
      showSolutions = showSolutionsCheckbox.checked,
      showClues = showCluesCheckbox.checked
    )

  /** add words from a chosen dictionary to the puzzle */
  def refineSolution(): Unit =
    val language = languageSelect.value
    val words = Globals.window(language).filter(_.length >= 4)
    refinedPuzzle = Puzzle.finalize(initialPuzzle, words.toList)
    renderSolution()

  /** show the print dialog */
  def printSolution(): Unit =
    dom.window.print()

  private def applyTagToInstruction(element: TextArea, tag: String): Unit =
    val start = element.selectionStart
    val end = element.selectionEnd
    val text = element.value
    val selectedText = text.substring(start, end)
    val newText = text.substring(0, start) + s"<$tag>$selectedText</$tag>" + text.substring(end)
    element.value = newText
    val newStart = start + tag.length + 2
    val newEnd = end + tag.length + 2
    element.setSelectionRange(newStart, newEnd)
    element.focus()
    renderSolution()

  /** normalize words and expand german umlauts */
  private def normalizeWord(word: String): String =
    word.trim.toUpperCase.
      replace("Ä", "AE").
      replace("Ö", "OE").
      replace("Ü", "UE").
      replace("ß", "SS")

