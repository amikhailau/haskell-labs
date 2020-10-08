# haskell-labs
## Lab1
			teilorsin:: Float->Int->Float (not working as expected)
			concatXX:: [[a]] -> [a]
			fmapX :: (a -> b) -> [a] -> [b]

## Lab2
			primeListX
			foldlX::(a->a->a)->a->[a]->a
			foldrX::(a->a->a)->a->[a]->a
			unfoldPrimeX::Int->Maybe(Int,Int)
			unfoldReverseBinaryX::Int->Maybe(Int, Int)
			
## Lab3
			Segment with code that fixes issues is in the end of file.
			Added binary operator nroot (nth root of number) - 2 nroot 4 = 2
					                                   3 nroot 8 = 3
			Added unary operator plus (increment by 1) - plus 2 = 3
								     plus 10 = 11

## Lab4
			New code is at the end of the file. Both something new and corrections to execRpn and runRpn.
			Two new operators were added: RpnUnOp UnOpJmp and RpnBinOp BinOpJmpif.
			UnOpJmp takes a number X from the top of the stack and jumps X commands forward or backward depending on sign.
			BinOpJmpif takes numbers X and Y from the top of the stack. If X == 0, then it jumps Y commands forward or backward depending on sign. 
			Otherwise it moves on without jumping. 
			If jump exceeds beginning of the operator stack, then it will jump to the beginning of the operator stack.
			If jump exceeds end of the operator stack, then it will jump to the end of the operator stack.
			Examples:
![Example UnOpJmp](Lab4/1.jpg?raw=true)
![Example BinOpJmpif](Lab4/2.jpg?raw=true)

			
