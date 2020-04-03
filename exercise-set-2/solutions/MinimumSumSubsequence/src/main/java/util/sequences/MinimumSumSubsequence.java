package util.sequences;

import java.util.Arrays;

/**
 * Implements algorithms for computing minimum sum subsequences.
 * 
 * @author scilingo
 */

public class MinimumSumSubsequence{

	/**
	* Computes a minimum sum subsequence by divide and conquer strategy.
	* @param sequence is an Integer sequence.
	* @return return a tuple containing,  value of the sum, the begin index
	* of subsequence and the end of the subsequence
	*/
	public static Tuple <Integer,Integer,Integer> minimumSumSubsequence (Integer[] sequence){
		Tuple<Integer, Integer, Integer> leftResult;
		Tuple<Integer, Integer, Integer> rightResult;
		Tuple<Integer, Integer, Integer> middleResult;

		int length = sequence.length;		
		if (length == 0) {
			return new Tuple<Integer,Integer,Integer>(0,-1,-1);
		}
		if (length == 1){
			if(sequence[0] < 0){
				return new Tuple<Integer,Integer,Integer>(sequence[0],0,0);
			}else{
				return new Tuple<Integer,Integer,Integer>(0,-1,-1);
			}
		}

		// split
		int half = length / 2;
		Integer[] leftHalf = Arrays.copyOfRange(sequence, 0 , half);
		Integer[] rightHalf = Arrays.copyOfRange(sequence, half , length);

		// conquer
		leftResult = minimumSumSubsequence(leftHalf);
		rightResult = minimumSumSubsequence(rightHalf);
		middleResult = minimumMiddleSum(sequence,half);

		if (rightResult.getSecond() != -1) {
			rightResult.setSecond( rightResult.getSecond() + half );
			rightResult.setThird( rightResult.getThird() + half );
		}
				
		// join
		if (middleResult.getFirst() < leftResult.getFirst() ) {
			if (middleResult.getFirst() < rightResult.getFirst() ) {
				return middleResult;
			}
			return rightResult;
		}
		if (leftResult.getFirst() < rightResult.getFirst()) {
			return leftResult;
		}
		return rightResult;
	}

	private static Tuple<Integer,Integer,Integer> minimumMiddleSum(Integer[] sequence, int half){
		int sum = 0;
		int minSum = 0;
		int lowerIndex = -1;
		for (int i = half-1; i >= 0; i--) {
			sum += sequence[i];
			if (sum < minSum){
				lowerIndex = i;
				minSum = sum;
			}
		}
		sum = minSum;
		int upperIndex = -1;
		for (int i = half; i < sequence.length; i++) {
			sum += sequence[i];
			if (sum < minSum){
				upperIndex = i;
				minSum = sum;
			}
		}
		if (minSum != 0) {
			if (lowerIndex == -1 ) {
				lowerIndex = half;
			}
			if (upperIndex == -1) {
				upperIndex = half - 1;
			}
		}

		return new Tuple(minSum, lowerIndex, upperIndex);

	}

}