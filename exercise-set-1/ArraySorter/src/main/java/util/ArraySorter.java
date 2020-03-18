package util;

/**
 * Sorts arrays of comparable objects using a variety of options.
 * 
 * @author aguirre
 *
 * @param <E> is the type of the elements of the array.
 */
public class ArraySorter<E extends Comparable<E>> {

	/**
	 * The array to sort.
	 */
	private E[] array;
	
	/**
	 * The algorithm to use for sorting.
	 */
	private SortAlgorithm algorithm = SortAlgorithm.INSERTIONSORT;

	/**
	 * Default constructor. Sets the array to sort and sorting algorithm to INSERTION SORT.
	 * @param array is the array to sort.
	 */
	public ArraySorter(E[] array) {
		if (array == null) throw new IllegalArgumentException("array must be non-null");
		this.array = array;
	}

	/**
	 * Constructor that sets array and sorting algorithm.
	 * @param array is the array to sort.
	 * @param algorithm is the algorithm to use for sorting.
	 */
	public ArraySorter(E[] array, SortAlgorithm algorithm) {
		if (array == null) throw new IllegalArgumentException("array must be non-null");
		this.array = array;
		this.algorithm = algorithm;
	}

	/**
	 * Sets the algorithm to use for sorting.
	 * @param algorithm is the algorithm to set for sorting.
	 */
	public void setAlgorithm(SortAlgorithm algorithm) {
		if (algorithm == null) throw new IllegalArgumentException("algorithm can't be null");
		this.algorithm = algorithm;
	}
	
	/**
	 * Retrieves the (sorted or yet unsorted) array within the ArraySorter.
	 * @return the array stored within the ArraySorter object.
	 */
	public E[] getArray() {
		return this.array;
	}

	/**
	 * Sets the array to be sorted.
	 * @param array is the new array to sort.
	 */
	public void setArray(E[] array) {
		throw new UnsupportedOperationException("method not yet implemented");		
	}

	/**
	 * Sorts the array.
	 * The array can then be retrieved using getArray() method.
	 */
	public void sort() {
		switch (this.algorithm) {
		case INSERTIONSORT: 	
			insertionSort(array); 
			break;
		case BUBBLESORT:
			bubbleSort(array); 
			break;
		case MERGESORT:
			mergeSort(array); 
			break;
		case SELECTIONSORT:
			selectionSort(array); 
			break;
		default:
            throw new UnsupportedOperationException("sorting method not yet implemented"); 
		}	
	}

	/**
	 * Sorts an array. Implements the selection sort algorithm.
	 * @param <T> is the type of the elements in the array.
	 * @param array is the array to be sorted.
	 */
	private static <T extends Comparable<T>> void selectionSort(T[] array) {
		if (array == null) throw new IllegalArgumentException("array is null, can't sort");
		int i = 0;
		while (i < array.length) {
			T min, aux;
			int minIndex, j;
			min = array[i];
			minIndex = i;
			j = i;
			while (j < array.length) {
				if (array[j].compareTo(min) < 0) {
					min = array[j];
					minIndex = j;
				}
				j++;
			}
			aux = array[i];
			array[i] = array[minIndex];
			array[minIndex] = aux;
			i++;
		}
	}

	/**
	 * Sorts an array. Implements the mergesort sort algorithm.
	 * @param <T> is the type of the elements in the array.
	 * @param array is the array to be sorted.
	 */
	private static <T extends Comparable<T>> void mergeSort(T[] array) {
		 //throw new UnsupportedOperationException("method not yet implemented");
		 if (begin < end){
			int mid = (begin + end)/2;
			mergeSort(array, begin, mid);//ordena la primera mitad
			mergeSort(array, mid+1, end);//ordena la segunda mitad
			merge(array, begin, mid, end);//mezcla las mitades ordenadas
		}
	}
	
	// merge: mezcla dos partes consecutivas de array
	// pre: 0 <= begin, mid, end <= array.lenght
	private static void merge(Comparable[] array, int begin, int mid, int end){
		 // busca los tamaños de los subarreglos
		 
        	int n1 = mid - begin + 1; 
        	int n2 = end - mid; 
  
        	/* crea 2 arreglos temporales */
        	Comparable L[] = new Comparable[n1]; 
        	Comparable R[] = new Comparable[n2]; 
  
        	/*copia los elementos a los arreglos temporales*/
        	
        	for (int i=0; i<n1; ++i) 
           		L[i] = array[begin + i];
           		
        	for (int j=0; j<n2; ++j) 
            		R[j] = array[mid +1+ j];  //   R[j] = arr[m + 1+ j]; 
            		
  
  
        	/* mezcla los arreglos temporales*/
  
        	// indices iniciales de los arreglos  temporales
        	int i = 0, j = 0; 
  
        	// indice inicial del  subarreglo mezclado 
        	int k = begin; 
        	while (i < n1 && j < n2){ 
            		if (L[i].compareTo(R[j])>=0){   // if l i <= r j
                		array[k] = L[i]; 
                		i++; 
            		} 
            		else{ 
                		array[k] = R[j]; 
                		j++; 
            			} 
            		k++; 
        	} 
  		
        	/* copia los elementos restantes de L[] si quedan */
        	while (i < n1){ 
        	
           		array[k] = L[i]; 
            		i++; 
            		k++;
            		
        	} 
  
        	/* copia los elementos restantes de  R[] si quedan */
        	while (j < n2){ 
        		
            		array[k] = R[j]; 
            		j++; 
            		k++; 
            		
        	} 
    	} 				
	// Swap: intercambia dos posiciones de un array
	// pre:  0 <= i,j <= array.lenght
	// post: intercambia los valores
	private static void swap(Comparable[] array, int i, int j){
	  Comparable temp = array[i];
  	  array[i] = array[j];
	  array[j] = temp;
 	}//end swap
 	
 	
	
	/**
	 * Sorts an array. Implements the bubblesort sort algorithm.
	 * @param <T> is the type of the elements in the array.
	 * @param array is the array to be sorted.
	 */
	private static <T extends Comparable<T>> void bubbleSort(T[] array) {
		//throw new UnsupportedOperationException("method not yet implemented");	
		boolean sorted = false;
	  	for (int pass = 1; (pass < n)&& !sorted; ++pass){
	    		sorted = true;
	    		for (int index = 0; index < n - pass; ++index){
	    			int nextIndex = index + 1;
	    			if (array[index].compareTo(array[nextIndex])>0){
		 			swap(array, index, nextIndex);
		  			sorted = false;
	    			} // end if
	    		}//end for
	  	}//end for
	}// end bubbleSort			
	
	/**
	 * Sorts an array. Implements the insertion sort algorithm.
	 * @param <T> is the type of the elements in the array.
	 * @param array is the array to be sorted.
	 */
	private static <T extends Comparable<T>> void insertionSort(T[] array) {
		//throw new UnsupportedOperationException("method not yet implemented");
		for ( int unsorted = 1; unsorted < n; unsorted++){ 
		// array [0.. unsorted −1] esta ordenado    
			Comparable nextItem = array [ unsorted ]; 
			int loc = unsorted ; 
			while ((loc > 0) && (array[loc-1].compareTo(nextItem) > 0)){
			         array[loc] = array[loc-1];         
			         loc--;     
			         }
			   //end while    
			array [ loc ] = nextItem ;     
		}//end for
	}//end insertionSort		
	}

	/**
	 * Checks if a given array is sorted.
	 * @param <T> is the type of the elements in the array.
	 * @param array is the array to be checked for sortedness.
	 * @return true iff the array is sorted.
	 */
	public static <T extends Comparable<T>> boolean isSorted(T[] array) {
		//throw new UnsupportedOperationException("method not yet implemented");
		boolean result = true;
		for (int i = 0; i < n; i++) {
			int j= i+1;
			// para evitar que se intente acceder a la posicion n (invalida del arreglo)
			if (j=n) {
				return result;
			} //if end
			//si no esta ordenado crecientemente sale
			if array[i]>array[j] {
				return = false;
			} //if end	
		} // for end
		return true;		
	}

}
