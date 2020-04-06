package algoritmos;

import static org.junit.Assert.assertTrue;
import static org.junit.Assert.assertEquals;

import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;

import org.junit.Test;


public class FibonacciTest {

  @Test
  public void testDynamicProg() {
    assertEquals(Fibonacci.fibonacciDC(3),Fibonacci.fibonacciProgDin(3));
  }

  @Test
  public void testMemoization() {
    assertEquals(Fibonacci.fibonacciDC(3),new Fibonacci().memoizedFib(3));
  }

  @Test(timeout=100)
  public void testPerformanceDP() {
    Fibonacci.fibonacciProgDin(4000000);
  }

  @Test(timeout=100, expected=java.lang.StackOverflowError.class)
  public void testPerformanceMemo() {
    new Fibonacci().memoizedFib(4000000);
  }

}
