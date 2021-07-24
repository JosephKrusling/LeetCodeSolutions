package dev.krusling.leetcode

object LC0002_AddTwoNumbers extends App {

  class ListNode(_x: Int = 0, _next: ListNode = null) {
    var next: ListNode = _next
    var x: Int = _x
  }

  // Inspired by: dustin10
  def addTwoNumbers(l1: ListNode, l2: ListNode): ListNode = {
    def add(l1: ListNode, l2: ListNode, carry: Boolean): ListNode = {
      val sum = (if (l1 == null) 0 else l1.x) +
        (if (l2 == null) 0 else l2.x) +
        (if (carry) 1 else 0)
      val out = new ListNode(sum % 10)

      // Recurse for any remaining nodes or carry flag
      if ((l1 != null && l1.next != null) || (l2 != null && l2.next != null) || sum >= 10) {
        out.next = add(if (l1 == null) null else l1.next, if (l2 == null) null else l2.next, sum >= 10)
      }

      out
    }

    add(l1, l2, carry = false)
  }


  /**
   * // Java solution
   * public ListNode addTwoNumbers(ListNode l1, ListNode l2) {
   * int carry = 0;
   * ListNode dummy = new ListNode(0);
   * ListNode p = dummy;
   * while (l1 != null || l2 != null || carry != 0) {
   * int sum = getOrZero(l1) + getOrZero(l2) + carry;
   * p.next = new ListNode(sum % 10);
   * p = p.next;
   * carry = sum / 10;
   *
   * l1 = getNextOrNull(l1);
   * l2 = getNextOrNull(l2);
   * }
   * return dummy.next;
   * }
   *
   * public int getOrZero(ListNode node) {
   * return (node == null) ? 0 : node.val;
   * }
   *
   * public ListNode getNextOrNull(ListNode node) {
   * return (node == null) ? null : node.next;
   * }
   *
   */


}
