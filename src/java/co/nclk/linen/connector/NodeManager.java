package co.nclk.linen.connector;

import java.util.HashMap;
import java.util.Map;
import java.util.UUID;


abstract class NodeManager {

	private interface NodeConnection {}
	private class NodeConnectionNotFoundException extends Exception {}

	private HashMap<String,NodeConnection> nodes = new HashMap<String,NodeConnection>();

	public NodeConnection getConnection(String name)
		throws NodeConnectionNotFoundException {

		if (nodes.containsKey(name))
			return nodes.get(name);
		throw new NodeConnectionNotFoundException();
	}

	public abstract NodeConnection createConnection(Map<String,Object> config, UUID runid);
}

