package funsets

/**
 * This class is a test suite for the methods in object FunSets.
 *
 * To run this test suite, start "sbt" then run the "test" command.
 */
class FunSetSuite extends munit.FunSuite:

  import FunSets.*

  test("contains is implemented") {
    assert(contains(x => true, 100))
  }

  /**
   * When writing tests, one would often like to re-use certain values for multiple
   * tests. For instance, we would like to create an Int-set and have multiple test
   * about it.
   *
   * Instead of copy-pasting the code for creating the set into every test, we can
   * store it in the test class using a val:
   *
   *   val s1 = singletonSet(1)
   *
   * However, what happens if the method "singletonSet" has a bug and crashes? Then
   * the test methods are not even executed, because creating an instance of the
   * test class fails!
   *
   * Therefore, we put the shared values into a separate trait (traits are like
   * abstract classes), and create an instance inside each test method.
   *
   */

  trait TestSets:
    val s1 = singletonSet(1)
    val s2 = singletonSet(2)
    val s3 = singletonSet(3)

  /**
   * This test is currently disabled (by using @Ignore) because the method
   * "singletonSet" is not yet implemented and the test would fail.
   *
   * Once you finish your implementation of "singletonSet", remove the
   * .ignore annotation.
   */
  test("singleton set one contains one") {

    /**
     * We create a new instance of the "TestSets" trait, this gives us access
     * to the values "s1" to "s3".
     */
    new TestSets:
      /**
       * The string argument of "assert" is a message that is printed in case
       * the test fails. This helps identifying which assertion failed.
       */
      assert(contains(s1, 1), "Singleton 1")
      assert(!contains(s1, 2), "Singleton 2")
  }

  test("union contains all elements of each set") {
    new TestSets:
      val s = union(s1, s2)
      assert(contains(s, 1), "Union 1")
      assert(contains(s, 2), "Union 2")
      assert(!contains(s, 3), "Union 3")
  }

  test("intersection test") {
    new TestSets:
      val s12 = union(s1, s2)
      val s23 = union(s2, s3)
      val interS = intersect(s12, s23)
      assert(contains(interS, 2), "intersection should contain common element")
      assert(!contains(interS, 1), "intersection should not contains element from set 1")
      assert(!contains(interS, 3), "intersection should not contain element from set 2")
  }

  test("diff test") {
    new TestSets:
      val s12 = union(s1, s2)

      assert(contains(diff(s12, s1), 2))
      assert(!contains(diff(s12, s1), 1))

      assert(contains(diff(s12, s3), 1))
      assert(contains(diff(s12, s3), 2))

      assert(contains(diff(s3, s12), 3))
  }

  test("filter test") {
    new TestSets:
      assert(contains(filter(s1, x => x > 0), 1))
      assert(!contains(filter(s1, x => x < 0), 1))
  }

  test("test forall") {
    new TestSets:
      val s = union(union(s1, s2), s3)
      assert(forall(s, x => x > 0))
      assert(!forall(s, x => x < 0))
      assert(!forall(s, x => x < 2))
  }

  test("test exists") {
    new TestSets:
      val s = union(union(s1, s2), s3)
      assert(exists(s, x => x > 0))
      assert(!exists(s, x => x < 0))
      assert(exists(s, x => x < 2))
  }

  test("test map") {
    new TestSets:
      val s = union(union(s1, s2), s3)
      val mapS = map(s, x => x * 2)
      assert(!contains(mapS, 1))
      assert(!contains(mapS, 3))
      assert(contains(mapS, 2))
      assert(contains(mapS, 4))
      assert(contains(mapS, 6))
  }


  import scala.concurrent.duration.*
  override val munitTimeout = 10.seconds
