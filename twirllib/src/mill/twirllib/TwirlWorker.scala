package mill
package twirllib

import java.io.File
import java.net.URLClassLoader
import java.{lang, util}

import ammonite.ops.{Path, ls}
import mill.eval.PathRef
import mill.scalalib.CompilationResult

import scala.io.Codec
import scala.reflect.runtime.universe._

class TwirlWorker {

  private var twirlInstanceCache = Option.empty[(Long, TwirlWorkerApi)]
  import scala.collection.JavaConverters._

  private def twirl(twirlClasspath: Agg[Path]) = {
    val classloaderSig = twirlClasspath.map(p => p.toString().hashCode + p.mtime.toMillis).sum
    twirlInstanceCache match {
      case Some((sig, instance)) if sig == classloaderSig => instance
      case _ =>
        val cpWithoutScalaLib = twirlClasspath.filter { p =>
          !p.toString.contains("scala-library")
        }.map(_.toIO.toURI.toURL).toArray
        val cl = new URLClassLoader(cpWithoutScalaLib, getClass.getClassLoader)
        val twirlCompilerClass = cl.loadClass("play.twirl.compiler.TwirlCompiler")

        val compileMethod = twirlCompilerClass.getMethod("compile",
          classOf[java.io.File],
          classOf[java.io.File],
          classOf[java.io.File],
          classOf[java.lang.String],
          cl.loadClass("scala.collection.Seq"),
          cl.loadClass("scala.collection.Seq"),
          cl.loadClass("scala.io.Codec"),
          classOf[Boolean]
        )

        val defaultAdditionalImportsMethod = twirlCompilerClass.getMethod("compile$default$5")
        val defaultConstructorAnnotationsMethod = twirlCompilerClass.getMethod("compile$default$6")
        val defaultCodecMethod = twirlCompilerClass.getMethod("compile$default$7")
        val defaultFlagMethod = twirlCompilerClass.getMethod("compile$default$8")

        val instance = new TwirlWorkerApi {
          override def compileTwirl(source: File,
                                    sourceDirectory: File,
                                    generatedDirectory: File,
                                    formatterType: String,
                                    additionalImports: Seq[String] = Nil,
                                    constructorAnnotations: Seq[String] = Nil) {
            val o = compileMethod.invoke(null, source,
              sourceDirectory,
              generatedDirectory,
              formatterType,
              additionalImports,
              constructorAnnotations,
              Codec("UTF-8"), // may provide as parameter too.
              scala.Boolean.box(false)
            )
          }
        }
        twirlInstanceCache = Some((classloaderSig, instance))
        instance
    }
  }

  def compile(twirlClasspath: Agg[Path], sourceDirectories: Seq[Path], dest: Path)
             (implicit ctx: mill.util.Ctx): mill.eval.Result[CompilationResult] = {
    val compiler = twirl(twirlClasspath)

    def compileTwirlDir(inputDir: Path) {
      ls.rec(inputDir).filter(_.name.matches(".*.scala.(html|xml|js|txt)"))
        .foreach { template =>
          val extFormat = twirlExtensionFormat(template.name)
          compiler.compileTwirl(template.toIO,
            inputDir.toIO,
            dest.toIO,
            s"play.twirl.api.$extFormat"
          )
        }
    }

    sourceDirectories.foreach(compileTwirlDir)

    val zincFile = ctx.dest / 'zinc
    val classesDir = ctx.dest / 'html

    mill.eval.Result.Success(CompilationResult(zincFile, PathRef(classesDir)))
  }

  private def twirlExtensionFormat(name: String) =
    if (name.endsWith("html")) "HtmlFormat"
    else if (name.endsWith("xml")) "XmlFormat"
    else if (name.endsWith("js")) "JavaScriptFormat"
    else "TxtFormat"
}

trait TwirlWorkerApi {
  def compileTwirl(source: File,
                   sourceDirectory: File,
                   generatedDirectory: File,
                   formatterType: String,
                   additionalImports: Seq[String] = Nil,
                   constructorAnnotations: Seq[String] = Nil)
}

object TwirlWorkerApi {

  def twirlWorker = new TwirlWorker()
}
