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
package org.neo4j.geoff.dsl

import org.neo4j.geoff.Descriptor
import java.util.ArrayList
import org.neo4j.geoff.store.NameableToken
import org.neo4j.geoff.store.Token.Type

object GeoffAST {

  trait Token { val typeName: String }
  case class NodeToken(name: String) extends Token { val typeName = "N" }
  case class RelationshipToken(name: String, relType: Option[String]) extends Token { val typeName = "R" }
  case class IndexToken(name: String) extends Token { val typeName = "I" }

  trait Rule
  case class NodeDefinitionRule(nodetoken: NodeToken, data: Map[String, Any], pattern: String = "N") extends Rule
  case class RelDefinitionRule(relToken: RelationshipToken, data: Map[String, Any], pattern: String = "R") extends Rule
  case class RelationshipRule(node1: NodeToken, rel: RelationshipToken, node2: NodeToken, data: Map[String, Any], pattern: String) extends Rule
  case class IndexRule(entity: Token, index: IndexToken, data: Map[String, Any], pattern: String = "I") extends Rule

  case class Rules(lis: Seq[Rule])

  /* Dirty Mapper for test purpose */
  object RuleHelp {
    import org.neo4j.geoff.{ Rule => GeoffRule }
    import org.neo4j.geoff.store.{ NodeToken => GeoffNodeToken }
    import org.neo4j.geoff.store.{ RelationshipToken => GeoffRelationshipToken }
    import org.neo4j.geoff.store.{ Token => GeoffToken }
    import org.neo4j.geoff.store.{ IndexToken => GeoffIndexToken }

    implicit def NodeTokentoGeoffNodeToken(nodeToken: NodeToken): GeoffNodeToken = {
      new GeoffNodeToken(nodeToken.name)
    }

    implicit def RelTokenToGeoffRelToke(relToken: RelationshipToken): GeoffRelationshipToken = {
      new GeoffRelationshipToken(relToken.name, relToken.relType.getOrElse(null))
    }

    implicit def RelTokenToGeoffRelToke(indexToken: IndexToken): GeoffIndexToken = {
      new GeoffIndexToken(indexToken.name)
    }

    implicit def immutableScalaMapToJavaMap(scalaMap: Map[String, Any]): java.util.Map[String, Object] = {
      import scala.collection.JavaConverters._
      val javaHashmap = new java.util.HashMap[String, Object]
      scalaMap map {case(x,y:Object) => javaHashmap.put(x, y)}
      javaHashmap
    }

    /* Dirty implicit converter for testing purpose */
    implicit def RuletoGeoffRule(rule: Rule): GeoffRule = {
      import scala.collection.JavaConversions._

      val geoffRule: GeoffRule = rule match {
        case nodeDefRule: NodeDefinitionRule =>
          val tokens = new ArrayList[GeoffToken]
          tokens add nodeDefRule.nodetoken
          new GeoffRule(new Descriptor("", tokens, nodeDefRule.pattern), nodeDefRule.data)

        case relDefRule: RelDefinitionRule =>
          val tokens = new ArrayList[GeoffToken]
          tokens add relDefRule.relToken
          new GeoffRule(new Descriptor("", tokens, relDefRule.pattern), relDefRule.data)

        case relationshipRule: RelationshipRule =>
          val tokens: ArrayList[GeoffToken] = relationshipRule.pattern match {

            case "N-R->N" =>
              val list = new ArrayList[GeoffToken]
              list add relationshipRule.node1
              list add new GeoffToken(GeoffToken.Type.CONNECTS)
              list add relationshipRule.rel
              list add new GeoffToken(GeoffToken.Type.CONNECTS)
              list add new GeoffToken(GeoffToken.Type.TO)
              list add relationshipRule.node2
              list

            case "N<-R-N" =>
              val list = new ArrayList[GeoffToken]
              list add relationshipRule.node1
              list add new GeoffToken(GeoffToken.Type.FROM)
              list add new GeoffToken(GeoffToken.Type.CONNECTS)
              list add relationshipRule.rel
              list add new GeoffToken(GeoffToken.Type.CONNECTS)
              list add relationshipRule.node2
              list

            case "N<-R->N" =>
              val list = new ArrayList[GeoffToken]
              list add relationshipRule.node1
              list add new GeoffToken(GeoffToken.Type.FROM)
              list add new GeoffToken(GeoffToken.Type.CONNECTS)
              list add relationshipRule.rel
              list add new GeoffToken(GeoffToken.Type.CONNECTS)
              list add new GeoffToken(GeoffToken.Type.TO)
              list add relationshipRule.node2
              list

          }
          new GeoffRule(new Descriptor("", tokens, relationshipRule.pattern), relationshipRule.data)

        case indexRule: IndexRule =>
          val tokens = new ArrayList[GeoffToken]
          indexRule.entity match {
            case entity: NodeToken =>
              tokens add entity
            case entity: RelationshipToken =>
              tokens add entity
          }
          tokens add new GeoffToken(GeoffToken.Type.IS_ENTRY_IN)
          tokens add indexRule.index
          new GeoffRule(new Descriptor("", tokens, indexRule.pattern), indexRule.data)
        // case _ => { Error handling code here }
      }
      geoffRule
    }
  }

}