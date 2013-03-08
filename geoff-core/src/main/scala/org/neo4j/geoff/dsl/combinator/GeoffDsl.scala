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

import scala.util.parsing.combinator._
import scala.util.parsing.combinator.syntactical._
import scala.util.parsing.combinator.syntactical.StandardTokenParsers
import org.neo4j.geoff.dsl.GeoffAST._
import scala.collection.immutable.Nil

object GeoffDsl extends StandardTokenParsers {

  lexical.delimiters += ("-", "[", "]", "<-", "->", "<=", "|", "(", ")", ",", "{", "}", ":")

  lazy val rules: Parser[Seq[Rule]] = rep(rule)

  lazy val rule: Parser[Rule] = indexRule | relRule | nodeRule

  /* Rules */

  lazy val relRule: Parser[Rule] =
    rel ~ (data?) ^^
      { case rel ~ data => RelDefinitionRule(rel, data.getOrElse(Map())) } |
    relationship

  lazy val indexRule =
    (rel | descriptor) ~ "<=" ~ index ~ (data?) ^^
      { case entity ~ "<=" ~ index ~ data => IndexRule(entity, index, data.getOrElse(Map()), entity.typeName + "^I") }

  lazy val nodeRule: Parser[Rule] = node

  /* Atoms */
  lazy val node: Parser[NodeDefinitionRule] =
    descriptor ~ (data?) ^^
      { case d ~ data => NodeDefinitionRule(d, data.getOrElse(Map())) }

  lazy val rel: Parser[RelationshipToken] =
    "[" ~> (ident?) ~ (relType?) <~ "]" ^^
      { case ident ~ relType => RelationshipToken(ident.getOrElse(""), relType) }

  lazy val relType =
    ":" ~> ident

  lazy val relationship: Parser[RelationshipRule] =
    (descriptor ~ ("-" | "<-") ~ rel ~ ("-" | "->") ~ descriptor ~ (data?)) ^^
      {
        case node1 ~ "-" ~ rel ~ "->" ~ node2 ~ data => RelationshipRule(node1, rel, node2, data.getOrElse(Map()), "N-R->N")
        case node1 ~ "<-" ~ rel ~ "-" ~ node2 ~ data => RelationshipRule(node1, rel, node2, data.getOrElse(Map()), "N<-R-N")
        case node1 ~ "<-" ~ rel ~ "->" ~ node2 ~ data => RelationshipRule(node1, rel, node2, data.getOrElse(Map()), "N<-R->N")
      }

  lazy val index: Parser[IndexToken] =
    "|" ~> ident <~ "|" ^^ IndexToken

  lazy val descriptor: Parser[NodeToken] =
    "(" ~> ident <~ ")" ^^ NodeToken

  lazy val data: Parser[Map[String, Any]] =
    "{" ~> repsep(entry, ",") <~ "}" ^^
      { case entry => Map() ++ entry }

  lazy val entry: Parser[(String, Any)] =
    stringLit ~ ":" ~ value ^^
      { case key ~ ":" ~ value => (key, value) }

  lazy val value =
    stringLit | numericLit | "null" | "true" | "false"
}