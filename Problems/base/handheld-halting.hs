module Main where

-- Source: https://bor0.wordpress.com/2020/12/10/advent-of-code-8/

import "base" Control.Monad
import "MissingH" Data.List.Utils
import "base" System.IO
import "containers" Data.Map

data Instruction = Nop | Acc | Jmp deriving (Show)
data Command = I Instruction Int deriving (Show)
type Program = [Command]

type Context = Map String Int

getEmptyCtx :: Context
getEmptyCtx = fromList [("acc", 0), ("IP", 0)]

eval :: Context -> Command -> Context
eval ctx (I Nop n) = incIP 1 ctx
eval ctx (I Acc n) = let acc = ctx ! "acc" in insert "acc" (acc + n) $ incIP 1 ctx
eval ctx (I Jmp n) = incIP n ctx

incIP :: Int -> Context -> Context
incIP n ctx = let ip = ctx ! "IP" in insert "IP" (ip + n) ctx

run :: Program -> Either (Context, [Int]) (Context, [Int])
run cmds = go cmds getEmptyCtx [] where
	go cs ctx prevIPs = let ip = ctx ! "IP" in go' ip where
		go' ip
			| ip >= length cs   = Right (ctx, prevIPs) -- if the instruction pointer out of bounds, the program terminated
			| elem ip prevIPs = Left (ctx, prevIPs)  -- if the instruction pointer was already executed, conclude infinite loop
			| otherwise  = let newctx = eval ctx (cs !! ip) in go cs newctx $ ip : prevIPs

example :: Program
example = I Acc 10 : I Nop 123 : I Jmp (-1) : []

main = print $ run example
