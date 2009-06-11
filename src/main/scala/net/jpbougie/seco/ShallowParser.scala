package net.jpbougie.seco

import net.jpbougie.bee.{Task, ExecutionProfile, Configurable}

import dispatch._
import dispatch.json._
import Js._

import net.lag.configgy.ConfigMap
import net.lag.logging.Logger

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.lexical._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.syntax._
import scala.util.parsing.input.CharArrayReader.EofCh

import java.io.{PrintStream, BufferedOutputStream, BufferedReader, InputStreamReader}

abstract class Element {
  def toJson: JsValue
}

case class Group(kind: String, atoms: List[Atom]) extends Element {
  def toJson = {
    val inskind = ('kind << kind)
    val inselems = ('elements << JsArray(atoms.map(_ toJson)))
    
    inskind(inselems(Js()))
  }
}
case class Atom(kind: String, label: String) extends Element {
  def toJson = {
    ('label << label)(('kind << kind)(Js()))
  }
}

trait InputTokens extends Tokens {
  case object OpenGroup extends Token             { val chars = "[" }
  case object CloseGroup extends Token            { val chars = "]" }
  case object OpenAtom extends Token              { val chars = "(" }
  case object CloseAtom extends Token             { val chars = ")" }
  case class Keyword(chars: String) extends Token { override def toString = chars }
}


class Lexer extends Lexical with InputTokens {
  def token: Parser[Token] = 
    ( '[' ^^^ OpenGroup
    | ']' ^^^ CloseGroup
    | '(' ^^^ OpenAtom
    | ')' ^^^ CloseAtom
    | rep1(elem("kw", ch => { ch != ' ' && ch != ')' && ch != EofCh})) ^^ { case elems => Keyword(elems mkString "") }
    | EofCh ^^^ EOF
    | failure("illegal character")
    )
      
  def whitespace = rep(whitespaceChar)
}

object Parser extends TokenParsers {
  type Tokens = InputTokens
  val lexical = new Lexer
  
  def kind = accept("kind", { case lexical.Keyword(kw) => kw })
  def root = rep1(group | atom)
  def group = accept(lexical.OpenGroup) ~> kind ~ rep1(atom) <~ accept(lexical.CloseGroup) ^^ { case kind ~ atoms => Group(kind, atoms) }
  def atom = accept(lexical.OpenAtom) ~> kind ~ accept("label", { case lexical.Keyword(kw) => kw }) <~ accept(lexical.CloseAtom) ^^ { case kind ~ label => Atom(kind, label)}
  
  def parse(input: String):Option[List[Element]] = 
    phrase(root)(new lexical.Scanner(input)) match {
          case Success(result: List[Element], _) => Some(result)
          case _ => None
        }
}

class ShallowParser extends Task with Configurable {
  
  var scriptPath: String = ""
  val logger = Logger.get
  
  def setup(config: ConfigMap): Unit = {
    this.scriptPath = config.getString("script_path", "parser.py")
  }
  
  def run(params: Map[String, ExecutionProfile]): JsValue = {
    logger.info("Running the shallow parser")
    val linex = 'question ? str
    val linex(question) = params("input").result.get
    
    logger.debug("Input : %s", question)
    
    val process = Runtime.getRuntime.exec(this.scriptPath)
    val writer = new PrintStream(new BufferedOutputStream(process.getOutputStream))
    
    new Thread() {
      override def run() = {
        writer.println(question)
        writer.flush
        writer.close
      }
    }.start
    
    
    logger.debug("Waiting for the process to complete")
    
    process.waitFor
    
    val reader = new BufferedReader(new InputStreamReader(process.getInputStream))
    
    val line = reader.readLine
    Console.println("Got " + line)
    
    Parser.parse(line) match {
      case Some(x) => JsArray(x.map(_ toJson))
      case _ => Js()
    }
  }
  
  override def identifier = "shallow"
}