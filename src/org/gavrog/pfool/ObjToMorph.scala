package org.gavrog.pfool

import org.gavrog.joss.meshes._
import Vectors._

import Document._

import scala.io.Source
import scala.collection.immutable.IntMap

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
			  current = current.update(n, new SparseVector(pairs :_*))
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
	  def largeEnough = pos.exists(x => x.abs > 1e-4)
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
	    doc("}").first insertBefore
			"""
				actor %s
				  {
				  channels
				    {
				    }
				  }
			""".trim.format(name)
	
	  doc("actor " + name).first
	}
	
	def writeDeltas(doc: Document, actorName: String, channelName: String,
			morphName: String, allDeltas: Seq[Delta], isPBM: Boolean) {
	  val deltas = allDeltas.filter(_.largeEnough)
	
	  findOrCreateActor(doc, actorName)("channels" \ "}").first insertBefore
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
	    		deltas.size, allDeltas.length, deltas.mkString("\n"))
	
	  if (isPBM) {
	    val ch = doc(("actor "+ actorName) \\ ("targetGeom "+ channelName)).first
	    ch("indexes").first.insertBefore(
	      """
					valueOpDeltaAdd
					Figure
					BODY
					%s
					deltaAddDelta 1
				"""
	      .trim.format(channelName)
	    )
	    ch("name").first.args = "PBM-" + morphName
	    ch("hidden").first.args = "1"
	    ch("forceLimits").first.args = "0"
	    ch("min").first.args = "-10000"
	    ch("max").first.args = "10000"
	  }
	}

	def main(args: Array[String]) {
		val original = new Mesh(Source fromFile args(0))
		val template =
			if (args(1) == "-s") new Mesh(Source fromFile args(2)) else null
		val weights =
			if (args(1) == "-w") readWeights(Source fromFile args(2)) else null
		val doc = blankPoseFile(6)
		val offset = if (template == null && weights == null) 0 else 2
		
		for (i <- (offset + 1) to args.size-1) {
		  val mesh        = new Mesh(Source fromFile args(i))
		  val morphName   = args(i).replaceFirst(".*/", "")
                               .replaceFirst("\\.obj$", "")
		  val channelName = "PBMCC_%02d" format (i - offset)
		
		  val morph  = if (template == null) mesh
			       else template.withMorphApplied(mesh).subdivision
		  val deltas = original.withDeltas(morph)
		  val actors = deltas.splitByGroup.filter(hasDeltas).toList
		  val isPBM  = actors.length > 1
		
		  if (isPBM)
		    findOrCreateActor(doc, "BODY")("channels" \ "}").first insertBefore
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
		    """.trim.format(channelName, morphName)
		
		  for (m <- actors) {
		    val actorName = m.groups.next.name
		    val data = if (weights == null) m.vertices.map(delta).toList else {
		      val current = weights(actorName)
		      (for (i <- 0 to current.lastKey if current.contains(i))
		         yield Delta(i, current(i).items
				        .map(e => deltas.vertex(e._1 + 1).pos * e._2).reduceLeft(_+_))
		      ).toList
		    }
		    writeDeltas(doc, actorName, channelName, morphName, data, isPBM)
		  }
		}
		
		doc.writeTo(System.out)
	}
}
