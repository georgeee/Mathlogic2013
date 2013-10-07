package ru.georgeee.mathlogic.provechecker.parser;

import java.util.ArrayList;
import java.util.HashMap;

/**
 * Created with IntelliJ IDEA.
 * User: georgeee
 * Date: 05.10.13
 * Time: 5:38
 * To change this template use File | Settings | File Templates.
 */
public class AhoCorasicTrie<T> {
    final TrieNode root;
    final ArrayList<TrieNode> trie = new ArrayList<TrieNode>();

    AhoCorasicTrie() {
        root = new TrieNode();
        trie.add(root);
        root.link = root;
    }

    void addWord(String word, T terminalVertexData) {
        char[] chars = word.toCharArray();
        TrieNode node = root;
        for (int i = 0; i < chars.length; ++i)
            node = node.getOrCreateNode(chars[i]);
        node.word = word;
        node.data = terminalVertexData;
    }

    public class Pair {
        public final String word;
        public final T data;

        public Pair(String word, T data) {
            this.word = word;
            this.data = data;
        }
    }

    public class TrieNode {
        protected T data;
        protected String word = null;
        protected TrieNode closestTerminalLinkage = null;
        protected HashMap<Character, TrieNode> goCache = null;
        protected HashMap<Character, TrieNode> childs = null;
        protected TrieNode link = null;
        protected final TrieNode parent;
        protected final char parentChar;

        //For root
        public TrieNode() {
            parent = null;
            parentChar = '\0';
        }

        public TrieNode(TrieNode parent, char parentChar) {
            this.parent = parent;
            this.parentChar = parentChar;
        }

        public boolean hasChild(char ch) {
            return childs != null && childs.containsKey(ch);
        }

        public TrieNode getOrCreateNode(char ch) {
            if (hasChild(ch)) return childs.get(ch);
            if (childs == null) childs = new HashMap<Character, TrieNode>();
            TrieNode node = new TrieNode(this, ch);
            trie.add(node);
            childs.put(ch, node);
            return node;
        }

        public TrieNode getLink() {
            if (link != null) return link;
            if (parent == root) return (link = root);
            return (link = parent.getLink().go(parentChar));
        }

        public TrieNode go(char ch) {
            if (goCache != null && goCache.containsKey(ch)) return goCache.get(ch);
            if (goCache == null) goCache = new HashMap<Character, TrieNode>();
            TrieNode result;
            if (hasChild(ch)) {
                result = childs.get(ch);
            } else {
                result = this == root ? root : getLink().go(ch);
            }
            goCache.put(ch, result);
            return result;
        }

        protected TrieNode getClosestTerminalLinkage() {
            if (closestTerminalLinkage != null) return closestTerminalLinkage;
            closestTerminalLinkage = getLink();
            while (closestTerminalLinkage != root && !closestTerminalLinkage.isTerminal())
                closestTerminalLinkage = closestTerminalLinkage.getLink();
            return closestTerminalLinkage;
        }

        protected Pair getNextPair(TrieNode node) {
            if (node.word != null) return new Pair(node.word, node.data);
            TrieNode _node = node.getClosestTerminalLinkage();
            if (_node.word == null) return null;
            return new Pair(_node.word, _node.data);
        }

//        /**
//         * Enumeration of matching words, in length decreasing order
//         *
//         * @return
//         */
//        public Enumeration<Pair> getMatchingWords() {
//            final TrieNode firstNode = word == null ? getClosestTerminalLinkage() : this;
//            return new Enumeration<Pair>() {
//                TrieNode node = firstNode;
//
//                @Override
//                public boolean hasMoreElements() {
//                    return node != root;
//                }
//
//                @Override
//                public Pair nextElement() {
//                    TrieNode _node = node;
//                    node = node.getClosestTerminalLinkage();
//                    return new Pair(_node.word, _node.data);
//                }
//            };
//        }

        public Pair getLongestMatchingPair() {
            return getNextPair(this);
        }

        public boolean isTerminal() {
            return word != null;
        }
    }


}
