/**
 * Copyright (c) 2002-2011 "Neo Technology,"
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

import org.junit.Assert;
import org.junit.Test;
import org.neo4j.geoff.RuleSet;
import org.neo4j.geoff.util.JSONException;


public class RuleSetTest {

	@Test
	public void testIfDescriptorFactoryUnderstandsRuleSets()
			throws Exception {
		String descString = "{\"(doc)\": {\"name\": \"doctor\"}, \"(dal)\": {\"name\": \"dalek\"}," +
				"\"(doc)-[:ENEMY_OF]->(dal)\": {\"since\": \"forever\"}, \"(doc)<=|People|\": {\"name\": \"The Doctor\"} }";
		RuleSet rules = RuleSet.from(descString);
		Assert.assertTrue(rules instanceof RuleSet);
		Assert.assertEquals(rules.length(), 4);
	}

	@Test(expected = JSONException.class)
	public void testIfDescriptorFactoryFailsOnRuleSetWithBadJSON()
			throws Exception {
		String descString = "{\"(doc)\": {\"name\"; \"doctor\"}, \"(dal)\": {\"name\": \"dalek\"}," +
				"\"(doc)-[:ENEMY_OF]->(dal)\": {\"since\": \"forever\"}, \"(doc)<=|People|\": {\"name\": \"The Doctor\"} }";
		RuleSet rules = RuleSet.from(descString);
		Assert.assertTrue(rules instanceof RuleSet);
		Assert.assertEquals(rules.length(), 4);
	}

}