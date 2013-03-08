/**
 * Copyright (c) 2002-2013 "Neo Technology,"
 * Network Engine for Objects in Lund AB [http://neotechnology.com]
 *
 * This file is part of Neo4j.
 *
 * Neo4j is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Affero General Public License as
 * published by the Free Software Foundation, either version 3 of the
 * License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Affero General Public License for more details.
 *
 * You should have received a copy of the GNU Affero General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package org.neo4j.geoff.dsl.combinator

import scala.util.parsing.input.StreamReader
import java.io.InputStreamReader
import scala.io.Source
import org.neo4j.geoff.Subgraph
import org.neo4j.test.ImpermanentGraphDatabase
import org.neo4j.geoff.Geoff
import GeoffDsl.rules
import org.neo4j.geoff.dsl.GeoffAST.RuleHelp.RuletoGeoffRule

object Main {

  def main(args: Array[String]) {

    for (line <- Source.fromInputStream(ClassLoader.getSystemResourceAsStream("matrix/The Matrix.geoff")).getLines())
      println(line)

    val subgraph:Subgraph = profile(parse)

    profile {
      val db:ImpermanentGraphDatabase = new ImpermanentGraphDatabase
      Geoff.insertIntoNeo4j(subgraph, db, null);
    }
  }

  def parse() : Subgraph =  {
    import GeoffDsl._
    import org.neo4j.geoff.dsl.GeoffAST.RuleHelp._

    rules(new lexical.Scanner(StreamReader(new InputStreamReader(ClassLoader.getSystemResourceAsStream("matrix/The Matrix.geoff"))))) match {
      case Success(rules, _) =>
          val subgraph:Subgraph = new Subgraph
          rules foreach { x =>
            println(x)
            subgraph.add(x)
          }
          subgraph
      case Failure(msg, _) => println("Failure: " + msg)
          new Subgraph //FIXME test purpose
      case Error(msg, _) => println("Error: " + msg)
          new Subgraph //FIXME test purpose
    }
  }

  def profile[R](code: => R): R = {
    val begin = System.currentTimeMillis
    val result = code
    val end = System.currentTimeMillis
    println("Elapsed time: " + (end - begin) + "ms")
    result
  }

}
