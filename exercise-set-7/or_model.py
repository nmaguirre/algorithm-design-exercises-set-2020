# Import library 
import numpy as np
from sklearn import linear_model
from sklearn.neural_network import MLPRegressor
from sklearn.metrics import accuracy_score

# Logical OR  / Data loading
X = [[0, 0], [0, 1], [1, 0], [1, 1]]
Y = [[0], [1], [1], [1]]

#train our algorithm with LinearRegression
lr = linear_model.LinearRegression()
lr.fit(X,Y)
print('Regression score:',lr.score(X,Y))

#train our algorithm with MLPRegressor
mlp = MLPRegressor(max_iter=2000, activation='identity', solver='sgd')
mlp.fit(X,np.ravel(Y))
print('MLPRegressor score:', mlp.score(X,Y))

# Testing final prediction 
print('Regression prediction Vs. MLPRegressor prediction')
for index in range(len(X)):
  print('')
  print("OR(" + str(X[index]) + ") = " + str(lr.predict([X[index]])))
  print("OR(" + str(X[index]) + ") = " + str(mlp.predict([X[index]])))    