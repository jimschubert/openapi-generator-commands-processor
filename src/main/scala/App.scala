import java.io.File
import java.nio.charset.Charset
import java.nio.file.Files

import scala.collection.concurrent.TrieMap
import scala.collection.mutable
import scala.util.control.Breaks._

sealed trait Argument {
  val short: String
  val long: String
  val jsonKey: String
  def jsonValue: String = null
}
case class Output(value: String) extends Argument {
  override val short: String = Output.short
  override val long: String = Output.long
  override val jsonKey: String = Output.jsonKey

  override def jsonValue: String = s""" "$value" """
}
object Output extends Argument {
  override val short: String = "-o"
  override val long: String = "--output"
  override val jsonKey: String =  """ "outputDir" """

  def unapply(input: Array[String]): Option[Output] = if (input.length == 2 && (input.head == short || input.head == long)) Option(Output(input.last)) else None
}

case class InputSpec(value: String) extends Argument {
  override val short: String = InputSpec.short
  override val long: String = InputSpec.long
  override val jsonKey: String = InputSpec.jsonKey

  override def jsonValue: String = s""" "$value" """
}
object InputSpec extends Argument{
  override val short: String = "-i"
  override val long: String = "--input-spec"
  override val jsonKey: String =  """ "inputSpec" """

  def unapply(input: Array[String]): Option[InputSpec] = if (input.length == 2 && (input.head == short || input.head == long)) Option(InputSpec(input.last)) else None
}

case class Generator(value: String) extends Argument {
  override val short: String = Generator.short
  override val long: String = Generator.long
  override val jsonKey: String = Generator.jsonKey

  override def jsonValue: String = s""" "$value" """
}

object Generator extends Argument{
  override val short: String = "-g"
  override val long: String = "--generator-name"
  override val jsonKey: String =  """ "generatorName" """

  def unapply(input: Array[String]): Option[Generator] = if (input.length == 2 && (input.head == short || input.head == long)) Option(Generator(input.last)) else None
}

case class AdditionalProperties(value: String) extends Argument {
  override val short: String = AdditionalProperties.short
  override val long: String = AdditionalProperties.long
  override val jsonKey: String = AdditionalProperties.jsonKey

  override def jsonValue: String = value.split(",").map(items => {
    val parts = items.split("=")
    s""" "${parts.head}": ${ if(List("true","false") contains parts(1)) parts(1) else "\"" + parts(1) + "\"" }"""
  }).mkString(",")
}
object AdditionalProperties extends Argument{
  override val short: String = "-p"
  override val long: String = "--additional-properties"
  override val jsonKey: String =  """ "additionalProperties" """

  def unapply(input: Array[String]): Option[AdditionalProperties] = if (input.length == 2 && (input.head == short || input.head == long)) Option(AdditionalProperties(input.last)) else None
}

case class TemplateDir(value: String) extends Argument {
  override val short: String = TemplateDir.short
  override val long: String = TemplateDir.long
  override val jsonKey: String = TemplateDir.jsonKey

  override def jsonValue: String = s""" "$value" """
}
object TemplateDir extends Argument{
  override val short: String = "-t"
  override val long: String = "--template-dir"
  override val jsonKey: String =  """ "templateDir" """

  def unapply(input: Array[String]): Option[TemplateDir] = if (input.length == 2 && (input.head == short || input.head == long)) Option(TemplateDir(input.last)) else None
}

case class ConfigFile(value: String) extends Argument {
  override val short: String = ConfigFile.short
  override val long: String = ConfigFile.long
  override val jsonKey: String = ConfigFile.jsonKey

  override def jsonValue: String = s""" "$value" """
}
object ConfigFile extends Argument{
  override val short: String = "-c"
  override val long: String = "--config"
  override val jsonKey: String =  """ "!include" """

  def unapply(input: Array[String]): Option[ConfigFile] = if (input.length == 2 && (input.head == short || input.head == long)) Option(ConfigFile(input.last)) else None
}

case class GenerateModelAsAlias(value: Boolean) extends Argument {
  override val short: String = GenerateModelAsAlias.short
  override val long: String = GenerateModelAsAlias.long
  override val jsonKey: String = GenerateModelAsAlias.jsonKey

  override def jsonValue: String = s""" ${if (value) "true" else "false" } """
}
object GenerateModelAsAlias extends Argument {
  override val short: String = "--generate-alias-as-model"
  override val long: String = "--generate-alias-as-model"
  override val jsonKey: String = """ "generateAliasAsModel" """

  def unapply(arg: String): Option[GenerateModelAsAlias] = if (arg == short) Option(GenerateModelAsAlias(true)) else None
}

case class ArtifactId(value: String) extends Argument {
  override val short: String = ArtifactId.short
  override val long: String = ArtifactId.long
  override val jsonKey: String = ArtifactId.jsonKey

  override def jsonValue: String = s""" "$value" """
}

object ArtifactId extends Argument{
  override val short: String = "--artifact-id"
  override val long: String = "--artifact-id"
  override val jsonKey: String =  """ "artifactId" """

  def unapply(input: Array[String]): Option[ArtifactId] = if (input.length == 2 && (input.head == short || input.head == long)) Option(ArtifactId(input.last)) else None
}

case class Library(value: String) extends Argument {
  override val short: String = Library.short
  override val long: String = Library.long
  override val jsonKey: String = Library.jsonKey

  override def jsonValue: String = s""" "$value" """
}

object Library extends Argument{
  override val short: String = "--library"
  override val long: String = "--library"
  override val jsonKey: String =  """ "library" """

  def unapply(input: Array[String]): Option[Library] = if (input.length == 2 && (input.head == short || input.head == long)) Option(Library(input.last)) else None
}


object Processor extends App {
  val outdir = new File("/Users/jim/projects/openapi-generator/bin/ci")
  val source = scala.io.Source.fromFile("/Users/jim/temp/commands-processor/commands.txt")
  val lines = try source.getLines.toList finally source.close()
  val counters = TrieMap.empty[String, Int]
  lines.filterNot(l => l.isEmpty).foreach { line =>
    val arguments = line.split("\t")
    val filename: String = arguments.head
    val parts = arguments.dropWhile(_ != "generate").drop(1)
    println(parts.mkString(" "))

    val groups = mutable.ArrayBuffer.empty[List[String]]
    var skip = false;
    var i = 0
    val last = parts.length - 1

    breakable {
      parts.foreach(p => {
        if (i == last && (skip || !p.startsWith("-"))) break
        val lookahead = if (i == last) "" else parts(i+1)

        if (!skip) {
          val partHasArg = p.startsWith("-") && lookahead != "" &&  !lookahead.startsWith("-")
          if (partHasArg) {
            // double args (options)
            val key = p
            val value = parts(i+1)

            println(s"Option: ${key} ${value}")
            groups.addOne(List(key, value))

            skip = true
          } else {
            // single arg (e.g. switches)
            // handles things like --library=ktor
            println(s"Single ${p}")
            if (p.startsWith("-") && p.contains("=")) {
              val eqIndex = p.indexOf("=")
              groups.addOne(List(p.substring(0, eqIndex), p.substring(eqIndex+1)))
            } else groups.addOne(List(p))

            skip = false
          }
        } else {
          // skip the current iteration (i.e. previous element was an option key, this iteration is the value.
          skip = false
        }

        i += 1
      })
    }

    val fileCount = counters.getOrElseUpdate(filename, 0)
    val arg: Seq[Argument] = groups.flatMap {
      case first :: second :: Nil => Array(first, second) match {
        case Output(i) => Option(i)
        case InputSpec(i) => Option(i)
        case Generator(i) => Option(i)
        case AdditionalProperties(i) => Option(i)
        case TemplateDir(i) => Option(i)
        case ConfigFile(i) => Option(i)
        case ArtifactId(i) => Option(i)
        case Library(i) => Option(i)
        case f =>
          println(s"Need extractor for: ${f.head}")
          Option.empty[Argument]
      }
      case first :: Nil => first match {
        case GenerateModelAsAlias(i) => Option(i)
        case f =>
          println(s"Need extractor for: ${f}")
          Option.empty[Argument]
      }
      case it =>
        println("NEED: " + it.head)
        Option.empty[Argument]
    }.toSeq

    val addl: Seq[Argument] = arg.filter(_.isInstanceOf[AdditionalProperties])
    val simple: Seq[Argument] = arg.filterNot(_.isInstanceOf[AdditionalProperties]).sortBy(_.jsonKey)

    val sb = new StringBuilder
    sb.append("{")

    var simpleIndex = 0
    val lastSimpleIndex = simple.length - 1
    simple.foreach(a => {
      sb.append(s"${a.jsonKey} : ${a.jsonValue} ")
      if (simpleIndex != lastSimpleIndex) sb.append(",")
      simpleIndex += 1
    })

    if (addl.nonEmpty) {
      sb.append(",")
      sb.append(s"${addl.head.jsonKey}:{")

      sb.append(addl.map(_.jsonValue).mkString(","))

      sb.append("}")
    }

    sb.append("}")

    println(s"$filename: ${sb.toString()}")

    val outFilename = s"${filename.replaceAllLiterally("./bin/", "").replace("/", "-").replace("-all", "").replace(".sh", "")}${(if (fileCount > 0) s"-$fileCount" else "")}.json"
//    println("\n\n" + sb.toString());
     val outfile = new File(outdir, outFilename)
     Files.write(outfile.toPath, sb.toString().getBytes(Charset.defaultCharset()))
    counters.update(filename, fileCount + 1)
    //      println(arg.mkString("|"))
  }
}