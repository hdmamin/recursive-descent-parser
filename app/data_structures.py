from typing import Optional, Any


class TrieNode:
    """Most information is stored in edges, the node itself just sort of glues edges together.

    node = TrieNode()
    node["d"] = TrieNode()
    node.edges

    # Out []: {'d': TrieNode(value=None, edges={})}

    node['d']['o'] = TrieNode("I am a leaf")
    # Out[]: TrieNode(value='I am a leaf', edges={})
    """

    def __init__(self, value: Optional[Any] = None, edges: dict[str, "TrieNode"] = None):
        """
        Parameters
        ----------
        value : any
            Non-null for leaf nodes. Some value that you want to associate with the sequence that
            led to this node.
        edges : dict[str, TrieNode]
        """
        self.value = value
        self.edges = edges or {}
 
    def __setitem__(self, key: str, node: "TrieNode"):
        """Add a new outgoing edge mapping a single character to a TrieNode object."""
        self.edges[key] = node
 
    def __getitem__(self, key: str):
        """Get a neighbor node by passing in a single character key that is in this node's outgoing
        edges.
        """
        return self.edges[key]
 
    def __contains__(self, key: str):
        """Check if a single character is in this node's outgoing edges."""
        return key in self.edges
 
    def __repr__(self):
        return f'{type(self).__name__}(value={self.value!r}, edges={self.edges})'


class Trie:
    """
    trie = Trie()
    trie.update({"dog": 1, "doggo": 2, "bird": 3})
    trie.get('dog')

    # Out[]: {
        'node': TrieNode(
            value=1,
            edges={'g': TrieNode(value=None, edges={'o': TrieNode(value=2, edges={})})}
        ),
        'is_leaf': True
    }

    trie.get('dogg')

    # Out[]: {
        'node': TrieNode(value=None, edges={'o': TrieNode(value=2, edges={})}),
        'is_leaf': False
    }

    trie.get('doggs')
    # raises KeyError
    """

    def __init__(self):
        self.root = TrieNode()
 
    def add(self, word: str, value: Any):
        """Add a word and corresponding value to the Trie. The value will be stored in the word's
        leaf node.
        """
        node = self.root
        for char in word:
            if char not in node:
                node[char] = TrieNode()
            node = node[char]
        node.value = value
 
    def update(self, items: dict, **kwargs):
        """Add multiple items to the Trie.

        Parameters
        ----------
        items : dict
            Maps words (keys) to values (some corresponding value that will be stored in the leaf
            node's `value` attribute).
        kwargs : str, Any
            Another way to pass in items. I.e. you can do
            trie.update(my_dict) or trie.update(**my_dict), much like a regular python dict.
        """
        for k, v in {**items, **kwargs}.items():
            self.add(k, v)
 
    def get(self, word: str) -> dict:
        """
        Returns
        -------
        dict
            Has keys "node" (TrieNode) and "is_leaf" (bool).
        """
        node = self.root
        try:
            for char in word:
                node = node[char]
            return {
                "node": node,
                "is_leaf": node.value is not None,
            }
        except KeyError as e:
            raise KeyError(f"{word} is not in trie: {e}")
 
    def __repr__(self):
        return f'{type(self).__name__}(root={self.root})'