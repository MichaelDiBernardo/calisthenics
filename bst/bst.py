import unittest


class BinarySearchTree(object):

    def __init__(self, value=None, left=None, right=None):
        self.value = value
        self.left = left
        self.right = right

    def contains(self, item):
        if self.value == item:
            return True

        if self._should_go_left(item):
            return self.left.contains(item)
        elif self._should_go_right(item):
            return self.right.contains(item)

        return False

    def insert(self, item):
        if self.value == None:
            self.value = item
        elif self._should_go_left(item):
            self.left.insert(item)
        elif self._should_go_right(item):
            self.right.insert(item)
        else:
            new_tree = BinarySearchTree(value=item)
            if item < self.value:
                self.left = new_tree
            else:
                self.right = new_tree

    def _should_go_left(self, item):
        return (item < self.value and self.left != None)

    def _should_go_right(self, item):
        return (item > self.value and self.right != None)

class EmptyBSTTest(unittest.TestCase):
    def setUp(self):
        self._sut = BinarySearchTree()

    def testCreateEmpty(self):
        self.assertFalse(self._sut.contains(1))


class SingleNodeBSTTest(unittest.TestCase):
    def setUp(self):
        self._sut = BinarySearchTree()
        self._sut.insert(1)

    def testSingleNodeContainsItem(self):
        self.assertTrue(self._sut.contains(1))

    def testSingleNodeDoesntContainItem(self):
        self.assertFalse(self._sut.contains(2))


class MultipleNodeBSTTest(unittest.TestCase):
    def setUp(self):
        self._sut = BinarySearchTree()
        for item in [2, 5, 4, 3, 7]:
            self._sut.insert(item)

    def testMultipleNodesContains(self):
        self.assertTrue(self._sut.contains(4))
        self.assertTrue(self._sut.contains(2))
        self.assertFalse(self._sut.contains(17))
        self.assertFalse(self._sut.contains(6))


if __name__ == "__main__":
    unittest.main()
