/**
 * Copyright (c) 2002-2012 "Neo Technology,"
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
package org.neo4j.geoff.test;

import org.junit.Test;
import org.neo4j.geoff.Subgraph;
import org.neo4j.geoff.store.IndexToken;
import org.neo4j.geoff.store.NodeToken;
import org.neo4j.geoff.store.RelationshipToken;
import org.neo4j.geoff.store.Token;

import java.util.List;

import static org.junit.Assert.*;

public class RuleTest {

	@Test
	public void canBuildNodeRule() throws Exception {
		String source = "(A) {\"name\": \"Alice\"}";
		List<Subgraph.Rule> rules = Subgraph.Rule.from(source);
		Subgraph.Rule rule = rules.get(0);
		assertNotNull(rule);
		assertEquals("N", rule.getDescriptor().getPattern());
		assertTrue(rule.getDescriptor().getToken(0) instanceof NodeToken);
		NodeToken token = (NodeToken) rule.getDescriptor().getToken(0);
		assertEquals(Token.Type.NODE, token.getTokenType());
		assertTrue(token.hasName());
		assertEquals("A", token.getName());
		assertTrue(rule.getData().containsKey("name"));
		assertEquals("Alice", rule.getData().get("name"));
	}

	@Test
	public void canBuildRelationshipRule() throws Exception {
		String source = "[:KNOWS] {\"since\": 1977}";
		List<Subgraph.Rule> rules = Subgraph.Rule.from(source);
		Subgraph.Rule rule = rules.get(0);
		assertNotNull(rule);
		assertEquals("R", rule.getDescriptor().getPattern());
		assertTrue(rule.getDescriptor().getToken(0) instanceof RelationshipToken);
		RelationshipToken relationshipToken = (RelationshipToken) rule.getDescriptor().getToken(0);
		assertEquals(Token.Type.REL, relationshipToken.getTokenType());
		assertFalse(relationshipToken.hasName());
		assertTrue(relationshipToken.hasType());
		assertEquals("KNOWS", relationshipToken.getType());
		assertTrue(rule.getData().containsKey("since"));
		assertEquals(1977, rule.getData().get("since"));
	}

	@Test
	public void canBuildPathRule() throws Exception {
		String source = "(A)-[:KNOWS]->(B) {\"since\": 1977}";
		List<Subgraph.Rule> rules = Subgraph.Rule.from(source);
		Subgraph.Rule rule = rules.get(0);
		assertNotNull(rule);
		assertEquals("N-R->N", rule.getDescriptor().getPattern());
		assertTrue(rule.getDescriptor().getToken(0) instanceof NodeToken);
		assertTrue(rule.getDescriptor().getToken(2) instanceof RelationshipToken);
		assertTrue(rule.getDescriptor().getToken(5) instanceof NodeToken);
		NodeToken startToken = (NodeToken) rule.getDescriptor().getToken(0);
		assertEquals(Token.Type.NODE, startToken.getTokenType());
		assertTrue(startToken.hasName());
		assertEquals("A", startToken.getName());
		RelationshipToken relationshipToken = (RelationshipToken) rule.getDescriptor().getToken(2);
		assertEquals(Token.Type.REL, relationshipToken.getTokenType());
		assertFalse(relationshipToken.hasName());
		assertTrue(relationshipToken.hasType());
		assertEquals("KNOWS", relationshipToken.getType());
		NodeToken endToken = (NodeToken) rule.getDescriptor().getToken(5);
		assertEquals(Token.Type.NODE, endToken.getTokenType());
		assertTrue(endToken.hasName());
		assertEquals("B", endToken.getName());
		assertTrue(rule.getData().containsKey("since"));
		assertEquals(1977, rule.getData().get("since"));
	}

	@Test
	public void canBuildNodeIndexEntryRule() throws Exception {
		String source = "(A)<=|People| {\"name\": \"Alice\"}";
		List<Subgraph.Rule> rules = Subgraph.Rule.from(source);
		Subgraph.Rule rule = rules.get(0);
		assertNotNull(rule);
		assertEquals("N^I", rule.getDescriptor().getPattern());
		assertTrue(rule.getDescriptor().getToken(0) instanceof NodeToken);
		assertTrue(rule.getDescriptor().getToken(2) instanceof IndexToken);
		NodeToken nodeToken = (NodeToken) rule.getDescriptor().getToken(0);
		assertEquals(Token.Type.NODE, nodeToken.getTokenType());
		assertTrue(nodeToken.hasName());
		assertEquals("A", nodeToken.getName());
		IndexToken indexToken = (IndexToken) rule.getDescriptor().getToken(2);
		assertEquals(Token.Type.INDEX, indexToken.getTokenType());
		assertTrue(indexToken.hasName());
		assertEquals("People", indexToken.getName());
		assertTrue(rule.getData().containsKey("name"));
		assertEquals("Alice", rule.getData().get("name"));
	}


	@Test
	public void canBuildRelationshipIndexEntryRule() throws Exception {
		String source = "[R]<=|People| {\"name\": \"Alice\"}";
		List<Subgraph.Rule> rules = Subgraph.Rule.from(source);
		Subgraph.Rule rule = rules.get(0);
		assertNotNull(rule);
		assertEquals("R^I", rule.getDescriptor().getPattern());
		assertTrue(rule.getDescriptor().getToken(0) instanceof RelationshipToken);
		assertTrue(rule.getDescriptor().getToken(2) instanceof IndexToken);
		RelationshipToken relationshipToken = (RelationshipToken) rule.getDescriptor().getToken(0);
		assertEquals(Token.Type.REL, relationshipToken.getTokenType());
		assertTrue(relationshipToken.hasName());
		assertEquals("R", relationshipToken.getName());
		assertFalse(relationshipToken.hasType());
		IndexToken indexToken = (IndexToken) rule.getDescriptor().getToken(2);
		assertEquals(Token.Type.INDEX, indexToken.getTokenType());
		assertEquals(true, indexToken.hasName());
		assertEquals("People", indexToken.getName());
		assertTrue(rule.getData().containsKey("name"));
		assertEquals("Alice", rule.getData().get("name"));
	}

	@Test
	public void canParseMultipleRules() throws Exception {
		String source = "(A) {\"name\": \"Alice\"} (B) {\"name\": \"Bob\"} (A)-[:KNOWS]->(B) {\"odd_value\": \"{#!:\\\"}\"} (C) (D) (C)-[:LIKES]->(D)";
		List<Subgraph.Rule> rules = Subgraph.Rule.from(source);
		assertEquals(6, rules.size());
		Subgraph.Rule aliceRule = rules.get(0);
		Subgraph.Rule bobRule = rules.get(1);
		Subgraph.Rule aliceKnowsBobRule = rules.get(2);
		Subgraph.Rule ruleC = rules.get(3);
		Subgraph.Rule ruleD = rules.get(4);
		Subgraph.Rule ruleCD = rules.get(5);
		assertEquals("(A)", aliceRule.getDescriptor().toString());
		assertEquals("Alice", aliceRule.getData().get("name"));
		assertEquals("(B)", bobRule.getDescriptor().toString());
		assertEquals("Bob", bobRule.getData().get("name"));
		assertEquals("(A)-[:KNOWS]->(B)", aliceKnowsBobRule.getDescriptor().toString());
		assertEquals("{#!:\"}", aliceKnowsBobRule.getData().get("odd_value"));
		assertEquals("(C)", ruleC.getDescriptor().toString());
		assertNull(ruleC.getData());
		assertEquals("(D)", ruleD.getDescriptor().toString());
		assertNull(ruleD.getData());
		assertEquals("(C)-[:LIKES]->(D)", ruleCD.getDescriptor().toString());
		assertNull(ruleCD.getData());
	}
	
}
