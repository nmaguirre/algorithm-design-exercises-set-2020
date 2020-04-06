package algoritmos;

import java.util.Map;
import java.util.HashMap;

/**
* Class that implements multiple solutions to calculate the n-esim number of
* the fibonacci serie. Using divide & conquer.
*/
public class Fibonacci {
	//Attribute of class
	Map<Integer,Integer> cache = new HashMap<Integer,Integer>();

	/**
	* Calculates the n-esim number in the fibonacci serie using classic
	* divide & conquer.
	*	@param n The place in the serie of the desired number.
	* @return The n-esim number of the fibonacci serie.
	* @throws IllegalArgumentException when the input is negative.
	*/
	public static int fibonacciDC(int n) throws IllegalArgumentException {
		if(n < 0) {
			throw new IllegalArgumentException("Cannot calculate fibonacci with negative numbers");
		}
		else {
			if(n == 0) {
				return 0;
			}
			else {
				if(n == 1) {
					return 1;
				}
				else {
					return fibonacciDC(n-1) + fibonacciDC(n-2);
				}
			}
		}
	}

	/**
	* Calculates the n-esim number in the fibonacci serie using dynamic
	* programming.
	*	@param n The place in the serie of the desired number.
	* @return The n-esim number of the fibonacci serie.
	* @throws IllegalArgumentException when the input is negative.
	*/
	public static int fibonacciProgDin(int n) throws IllegalArgumentException {
		throw new UnsupportedOperationException("Method not yet implemented!");
	}

	/**
	* Calculates the n-esim number in the fibonacci serie using memoization.
	*	@param n The place in the serie of the desired number.
	* @return The n-esim number of the fibonacci serie.
	* @throws IllegalArgumentException when the input is negative.
	*/
	public int memoizedFib(int n) throws IllegalArgumentException {
		throw new UnsupportedOperationException("Method not yet implemented!");
	}

	/**
	* Auxiliar method for memoization technique
	* if a element isn't in the cache, it puts on it, else
	* return the mapped element in the cache.
	* @param n The key of the cache.
	* @return The mapped value.
	*/
	private int memoFib(int n) {
		throw new UnsupportedOperationException("Method not yet implemented!");
	}

}
