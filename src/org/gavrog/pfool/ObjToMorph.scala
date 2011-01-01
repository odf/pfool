package org.gavrog.pfool

import org.gavrog.joss.meshes._
import Vectors._

import Document._

import scala.io.Source
import collection.immutable.{Queue, IntMap}

object ObjToMorph {
  implicit def asSource(s: String) = Source.fromString(s)
  implicit def asLine(s: String) = new Line(s)

  def readWeights(source: Source) = {
		var result = Map[String, IntMap[SparseVector]]()
		var name: String = null
	  var current = IntMap[SparseVector]()
				
	  for {
		  raw <- source.getLines
		  line = raw.trim
		  if line.length > 0 && !line.startsWith("#")
	  } {
		  val fields: Seq[String] = line.split("\\s+")
		  val cmd = fields(0)
		  val pars = fields.slice(1, fields.length)
				
		  cmd match {
		  case "actor" => {
			  if (current.size > 0) result += (name -> current)
			  name = pars.mkString(" ")
			  current = if (result.contains(name)) result(name)
			  else IntMap[SparseVector]()
		  }
		  case "w" => {
			  val n = pars(0).toInt
			  val k = pars(1).toInt
			  val pairs = (1 to k).map(i => (pars(i+1).toInt, pars(i+k+1).toDouble))
			  current = current.updated(n, new SparseVector(pairs :_*))
		  }
		  case _ => println("?? " + cmd + "(" + pars.mkString(", ") + ")")
	  	}
	  }
	  if (current.size > 0) result += (name -> current)
	  result
  }

	case class Delta(nr: Int, pos: Vec3) {
	  override def toString =
		  "d %d %.8f %.8f %.8f".format(nr, pos.x, pos.y, pos.z)
	  def largeEnough = pos.toList.exists(x => x.abs > 1e-5)
    def scaled(f: Double) = Delta(nr, pos * f)
	}

	def delta(v: Mesh.Vertex) = Delta(v.nr-1, v.pos)
	def hasDeltas(m: Mesh) = m.vertices.exists(v => delta(v).largeEnough)

	def blankPoseFile(version: Any) = new Document("""
	  {
	  version
	    {
	    number %s
	    }
	  }
	  {
	  version
	    {
	    number %s
	    }
	  figure
	    {
	    }
	  }""".trim.format(version, version))
	
	def findOrCreateActor(doc: Document, name: String) = {
	  if (doc("actor " + name).isEmpty)
	    doc("}").head insertBefore
			"""
				actor %s
				  {
				  channels
				    {
				    }
				  }
			""".trim.format(name)
	
	  doc("actor " + name).head
	}
	
	def writeDeltas(doc: Document, actorName: String, channelName: String,
			morphName: String, allDeltas: Seq[Delta], isPBM: Boolean, factor: Double) {
	  val deltas = allDeltas.filter(_.largeEnough)
	
	  findOrCreateActor(doc, actorName)("channels" \ "}").head insertBefore
	  """
	    targetGeom %s
	      {
	      name %s
	      initValue 0
	      hidden 0
	      forceLimits 1
	      min 0
	      max 1
	      trackingScale 0.02
	      keys
					{
					static 0
					k 0 0
					}
	      interpStyleLocked 0
	      indexes %d
	      numbDeltas %d
	      deltas
					{
	    	  %s
					}
	      blendType 0
	      }
	    """.trim.format(channelName, morphName,
	    		deltas.size, allDeltas.length, deltas.map(_.scaled(factor)).mkString("\n"))
	
	  if (isPBM) {
	    val ch = doc(("actor "+ actorName) \\ ("targetGeom "+ channelName)).head
	    ch("indexes").head.insertBefore(
	      """
					valueOpDeltaAdd
					Figure
					BODY
					%s
					deltaAddDelta 1
				"""
	      .trim.format(morphName)
	    )
	    ch("name").head.args = "PBM-" + morphName
	    ch("hidden").head.args = "1"
	    ch("forceLimits").head.args = "0"
	    ch("min").head.args = "-10000"
	    ch("max").head.args = "10000"
	  }
	}

	def makeMorph(doc: Document, morphName: String, channelNr: Int,
                deltas: Mesh, weights: Map[String, IntMap[SparseVector]],
                factor: Double, asExpression: Boolean)
  {
    def extra(m: Mesh) =
      if (asExpression) m.groups.head.name == "head" else true
    val actors = deltas.splitByGroup.filter(hasDeltas).filter(extra).toList
    val isPBM = actors.length > 1
		val channelName =
      if (asExpression) morphName else "PBMCC_%02d" format channelNr

    if (isPBM) {
      findOrCreateActor(doc, "BODY")("channels" \ "}").head insertBefore
              """
             valueParm %s
               {
               name %s
               initValue 0
               hidden 0
               forceLimits 1
               min 0
               max 1
               trackingScale 0.02
               keys
                 {
                 static 0
                 k 0 0
                 }
               interpStyleLocked 0
               blendType 0
               }
           """.trim.format(morphName, morphName)
      doc("version").head.insertAfter("createFullBodyMorph " + morphName)
    }

    for (m <- actors) {
      val actorName = m.groups.head.name
      val data = if (weights == null) m.vertices.map(delta).toList else {
        val current = weights(actorName)
        (for (i <- 0 to current.lastKey if current.contains(i))
        yield Delta(i, current(i).items
                  .map(e => deltas.vertex(e._1 + 1).pos * e._2).reduceLeft(_ + _))
                ).toList
      }
      writeDeltas(doc, actorName, channelName, morphName, data, isPBM, factor)
    }
  }


  def main(args: Array[String]) {
    var arguments = Queue[String]()
    var template: Mesh = null
    var weights: Map[String, IntMap[SparseVector]] = null
    var factor = 1.0
    var split = false
    var margin = 0.0
    var asExpression = false

    var i = 0
    while (i < args.size) {
      args(i) match {
        case "-s" => {
          i += 1
          template = new Mesh(Source fromFile args(i))
        }
        case "-w" => {
          i += 1
          weights = readWeights(Source fromFile args(i))
        }
        case "-f" => {
          i += 1
          factor = args(i).toDouble
        }
        case "-m" => {
          i += 1
          split = true
          margin = args(i).toDouble
        }
        case "-x" => asExpression = true
        case _ => arguments += args(i)
      }
      i += 1
    }

		val original = new Mesh(Source fromFile arguments.head)
		val doc = blankPoseFile(6)
    var channelNr = 1

		for (arg <- arguments.tail) {
		  val mesh      = new Mesh(Source fromFile arg)
		  val morphName = arg.replaceFirst(".*/", "").replaceFirst("\\.obj$", "")
		
		  val morph  = if (template == null) mesh
			       else template.withMorphApplied(mesh).subdivision
		  val deltas = original.withDeltas(morph)

      makeMorph(doc, morphName, channelNr, deltas,
        weights, factor, asExpression)
      channelNr += 1

      if (split) {
        val left = deltas.clone
        val right = deltas.clone
        for (v <- original.vertices) {
          val delta = deltas.vertex(v.nr).pos
          if (v.pos.x < -margin) {
            left.vertex(v.nr).pos  = zero3
            right.vertex(v.nr).pos = delta
          }
          else if (v.pos.x < margin) {
            left.vertex(v.nr).pos  = delta * (margin + v.pos.x) / (2 * margin)
            right.vertex(v.nr).pos = delta * (margin - v.pos.x) / (2 * margin)
          } else {
            left.vertex(v.nr).pos = delta
            right.vertex(v.nr).pos = zero3
          }
        }

        makeMorph(doc, morphName + "Left", channelNr, left,
          weights, factor, asExpression)
        makeMorph(doc, morphName + "Right", channelNr+1, right,
          weights, factor, asExpression)
        channelNr += 2
      }
		}
		
		doc.writeTo(System.out)
	}
}
