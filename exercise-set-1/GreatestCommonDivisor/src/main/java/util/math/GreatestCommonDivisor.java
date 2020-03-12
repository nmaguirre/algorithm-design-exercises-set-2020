package util.math;

import java.util.ArrayList;

/**
 * Computeds greatest common divisor of two nonnegative, not-both-zero
 * integers using diferents algorithms.
 * 
 * @author scilingo
 */

public class GreatestCommonDivisor {

	/**
	* Computes greatest common divisor by Euclid's algorithm
	* @param m is a nonnegative integer fisrt argument.
	* @param n is second nonnegative integer argument.
	* @return the greatest common divisor between m and n.
	*/
	public static int euclidAlgorithm(int m, int n){
		if (m < 0 || n < 0 || (m == 0 && n == 0)) throw new IllegalArgumentException("numbers must be nonnegative and not-both-zero");
		if(n == 0)
			return m;
		return euclidAlgorithm(n,m%n);
	}

	/**
	* Computes greatest common divisor by definition based algorithm
	* @param m is a nonnegative integer fisrt argument.
	* @param n is second nonnegative integer argument.
	* @return the greatest common divisor between m and n.
	*/
	public static int definitionBasedAlgorithm(int m, int n){
		throw new UnsupportedOperationException("method not yet implemented");
	}

	/**
	* Computes greatest common divisor by middle school procedure
	* @param m is a nonnegative integer fisrt argument.
	* @param n is second nonnegative integer argument.
	* @return the greatest common divisor between m and n.
	*/
	public static int middleSchoolAlgorithm(int m, int n){
		throw new UnsupportedOperationException("method not yet implemented");
	}

	/**
	* Implements the sieve of Eratosthenes
	* @param n is a number greater than 1
	* @return Array of all prime numbers less than or equal to n.
	*/
	private static int[] sieve(int n){
		throw new UnsupportedOperationException("method not yet implemented");
	}


}