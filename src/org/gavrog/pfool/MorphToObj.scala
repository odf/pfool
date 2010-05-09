package org.gavrog.pfool

import scala.io.Source
import scala.collection.immutable.IntMap

import org.gavrog.joss.meshes._
import Vectors._

import Document._

object MorphToObj {
	def verticesByGroup(m: Mesh) = Map() ++ m.groups.map(g =>
	  (g.name, {
	    val faces = m.faces.filter(f => f.group == g).toList
	    val verts = Set() ++ faces.flatMap(_.vertices)
	    m.vertices.filter(null !=).filter(verts contains).toList
	  }))
	
  def main(args: Array[String]) {
		val mesh = new Mesh(Source fromFile args(0))
		val doc  = Document.fromFile(args(1))
		val name = args(2)
		val parts = verticesByGroup(mesh)
		
		val byGroup =
		  for { actor  <- doc("actor")
		       channel <- actor(() \\ ("targetGeom " + name) \@ "deltas")
		  } yield {
		    System.err.println("actor " + actor.args + ", channel " + channel.args)
		    val groupName = actor.args.split(":")(0)
		    val part = parts(groupName).toSeq
		    val data = IntMap[Vec3]() ++ channel("deltas" \ "d").map(line => {
		      val pars = line.args.split("\\s+")
		      val coords = pars.drop(1).map(_.toDouble)
		      (part(pars(0).toInt).nr -> new Vec3(coords(0), coords(1), coords(2)))
		    })
		    (groupName -> data)
		  }
		
		val byVertex = byGroup.flatMap(_._2).foldLeft(
		  IntMap[List[Vec3]]())((m, e) => {
			  val (i, v) = e
			  m.update(i, v :: m.getOrElse(i, List[Vec3]()))
		  })
		
		for ((i, list) <- byVertex)
		  mesh.vertex(i).pos += list.reduceLeft(_+_) / list.size
		
		mesh.write(System.out, name)
	}
}
