package util.math;

import java.util.ArrayList;

/**
 * This class implements several algorithms to compute Matrix Chain
 * Multiplication problem. Wich consists to find the right parenthesize
 * of a matrices chain. In order to compute the product of a matrices
 * chain with the minimum number of multiplication operations.
 */
public class MatrixChainMultiplication
{
    private Integer[] chain;
    private int [][] p;

	public MatrixChainMultiplication(Integer[] chain){
		this.chain = chain;
		this.p = new int [chain.length][chain.length];
	}

	public String getParenthesized(){

		return buildParenthesized(1,chain.length-1);
	}

	private String buildParenthesized(int i, int j){

	}


	public int mcm(Integer[] chain, int i, int j){

	}




}
