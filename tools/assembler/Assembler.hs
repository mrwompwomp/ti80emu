import Control.Arrow
import Data.Bits
import Data.List
import Debug.Trace
import System.Environment
import System.Exit

processArgs ["-3",a] = return (3,a)
processArgs ["-4",a] = return (4,a)
processArgs [a@(c:_)] | c /= '-' = return (4,a)
processArgs as = do
	p <- getProgName
	putStrLn $ "Usage: " ++ p ++ " [-3|-4] file"
	if null as then exitSuccess else exitFailure

main = do
	(version,file) <- getArgs >>= processArgs
	xs <- fmap (map words . lines) $ readFile (file ++ ".asm")
	preprocess xs >>= writeFile (file ++ ".80z") . unlines . assemble version
preprocess xs = fmap reverse $ f [] xs where
	f xs [] = return xs
	f xs (("INCLUDE":x):ys) = do
		zs <- fmap (map words . lines) $ readFile $ unwords x
		f xs (zs ++ ys)
	f xs (y:ys) = f (y:xs) ys

h a = reverse $ f 4 a where
	f 0 _ = ""
	f i a = hs !! (a `mod` 16):f (i-1) (a `div` 16)
	hs = "0123456789ABCDEF"
addr d x = uh $ maybe x id $ lookup x d
loc _ "A" = 0x0FF
loc _ "DP" = 0x11C
loc _ "I" = 0x100
loc _ "SP" = 0x118
loc _ "(I)" = 0x1FF
loc d ('(':x) = reg d (init x)
reg d x = f (maybe y uh (lookup x' d)) i' where
	y = if head x' == 'J' then 0x200 + uh (tail x') else uh x'
	(x',i) = break (\c -> c == '+' || c == '-') x
	i' = if null i then 0 else
		(if head i == '+' then id else negate) $ read ('0':'x':tail i)
	f x y = x .&. 0x1F0 .|. (x + y) .&. 0x00F
flag d x = maybe x id $ lookup x d
valLE d x = let y = val d x in y `shiftL` 8 .&. 0xFF00 .|. y `shiftR` 8
val d x = uh (maybe x' id (lookup x' d)) + i' where
	(x',i) = break (\c -> c == '+' || c == '-') x
	i' = if null i then 0 else
		(if head i == '+' then id else negate) $ read ('0':'x':tail i)
val' d x = maybe x' id $ lookup x' d where
	x' = takeWhile (\c -> c /= '+' && c /= '-') x
uh x = case reads ('0':'x':x) of
	[(x,"")] -> x
	_ -> error $ "unknown name " ++ x
line (_,a) x y zs = first (l:) where
	l = case y of
		Nothing -> h a ++ ':':' ':h x ++ "      " ++ unwords zs
		Just y -> h a ++ ':':' ':h x ++ ' ':h y ++ ' ':unwords zs

jo ("CALL":_) = 0x0200
jo ("JUMP":_) = 0x0200
jo ("IF":"NO":"CARRY:":_) = 0x0400
jo ("IF":"CARRY:":_) = 0x0600
jc d ("CALL":x:_) = 0x8000 + addr d x
jc d ("JUMP":x:_) = addr d x
jc d ("IF":"NO":"CARRY:":xs) = jc d xs
jc d ("IF":"CARRY:":xs) = jc d xs

cmp '=' = 0x0400
cmp '≠' = 0x0600
cmp '<' = 0x0C00
cmp '≥' = 0x0E00

size [_] = 0x0100
size [_,_] = 0x0000

yx x = x `shiftL` 4 .&. 0xF0 .|. x `shiftR` 4 .&. 0x0F

translate 4 x = x
translate 3 x = h $ f $ uh x where
	f a | a < 0x04C2 = a
	f a | a < 0x04C6 = error $ "address " ++ h a ++ " not in ROM 3.0"
	f a | a < 0x0DC2 = a - 0x4
	f a | a < 0x0DC8 = error $ "address " ++ h a ++ " not in ROM 3.0"
	f a | a < 0x13CF = a - 0xA
	f a | a < 0x179C = a - 0x7
	f a | a < 0x179D = error $ "address " ++ h a ++ " not in ROM 3.0"
	f a | a < 0x17A1 = a - 0x8
	f a | a < 0x17A3 = error $ "address " ++ h a ++ " not known in ROM 3.0"
	f a | a < 0x17A8 = a - 0x9
	f a | a < 0x17A9 = error $ "address " ++ h a ++ " not in ROM 3.0"
	f a | a < 0x17B2 = a - 0xA
	f a | a < 0x17B3 = error $ "address " ++ h a ++ " not in ROM 3.0"
	f a | a < 0x17B5 = a - 0xB
	f a | a < 0x17B6 = error $ "address " ++ h a ++ " not in ROM 3.0"
	f a | a < 0x17B7 = a - 0xC
	f a | a < 0x17B8 = error $ "address " ++ h a ++ " not in ROM 3.0"
	f a | a < 0x17CC = a - 0xD
	f a | a < 0x17CD = error $ "address " ++ h a ++ " not in ROM 3.0"
	f a | a < 0x17D0 = a - 0xE
	f a | a < 0x17D1 = error $ "address " ++ h a ++ " not in ROM 3.0"
	f a | a < 0x17D8 = a - 0xF
	f a | a < 0x17D9 = error $ "address " ++ h a ++ " not in ROM 3.0"
	f a | a < 0x17DA = a - 0x10
	f a | a < 0x17DB = error $ "address " ++ h a ++ " not in ROM 3.0"
	f a | a < 0x17DC = a - 0x11
	f a | a < 0x17DE = error $ "address " ++ h a ++ " not in ROM 3.0"
	f a | a < 0x17E3 = a - 0x13
	f a | a < 0x17E4 = error $ "address " ++ h a ++ " not in ROM 3.0"
	f a | a < 0x17E8 = a - 0x14
	f a | a < 0x17E9 = error $ "address " ++ h a ++ " not in ROM 3.0"
	f a | a < 0x17ED = a - 0x15
	f a | a < 0x17EE = error $ "address " ++ h a ++ " not in ROM 3.0"
	f a | a < 0x1FFF = a - 0x16
	f a | a < 0x4000 = error $ "address " ++ h a ++ " not in ROM"
	f a | a < 0x4EAD = a
	f a | a < 0x7CC5 = a + 0xD
	f a | a < 0x7FF1 = a + 0xE
	f a | a < 0x7FF2 = error $ "address " ++ h a ++ " not in ROM 3.0"
	f a | a < 0x7FFF = a - 0x3145
	f a | a < 0x8000 = a
	f a = f (a - 0x8000) + 0x8000

testMSB (_,a) i x = first $
	if x .&. 0x0F0 == 0x0F0 then id
	else if i == 2 && x `shiftR` 8 /= 2 then
		trace (h a ++ ": ambiguous register index (MSB used by CPU unknown)") id
	else if i /= 2 && x `shiftR` 8 == i then
		error $ h a ++ ": bad register index (CPU would use opposite MSB)"
	else id

instance (Num a, Num b) => Num (a, b) where
	~(a,b) + ~(c,d) = (a + c,b + d)
	(-) = error "(-)"
	(*) = error "(*)"
	abs = error "abs"
	signum = error "signum"
	fromInteger x = (fromInteger x,fromInteger x)

assemble v xs = let (r,d) = f (2 :: Int) addrError d xs in r where
	addrError = (x,x) where x = error "no base address specified"
	f _ _ _ [] = ([],[])
	f i a d ([]:ls) = f i a d ls
	f i a d ((('#':_):_):ls) = f i a d ls
	f i _ d (("ORG":a:_):ls) = f i (addr d a,addr d a) d ls
	f i (a,_) d (("RELOCATE":b:_):ls) = f i (a,addr d b) d ls
	f i (a,_) d (("END":"RELOCATION":_):ls) = f i (a,a) d ls
	f i (a,b) d ((":":x:_):ls) = (((h b ++ '(':x ++ "):"):) *** ((x,h b):)) $
	 f 2 (a,b) d ls
	f i a d (l@("NAME":x:y:_):ls) = second ((y,x):) $ f i a d ls
	f i a d (l@("ROM":"NAME":x:y:_):ls) = second ((y,translate v x):) $ f i a d ls
	f i a d (l@("ADD":x:xs):ls)
		| last x == ';' = line a (op + jo xs) (Just $ jc d xs) l $ f i (a+2) d ls
		| otherwise = line a op Nothing l $ f i (a+1) d ls
	 where
		(u,',':v) = break (',' ==) (if last x == ';' then init x else x)
		t = loc d u
		op | u == "(I)" = 0xE000 .|. size (val' d v) .|. val d v .&. 0xFF
		   | head x == 'A' = 0xF000 .|. size (val' d v) .|. val d v .&. 0xFF
		   | otherwise =
			0xC000 .|. t `shiftL` 4 .&. 0x01F0 .|. t `shiftL` 7 .&. 0x1000 .|.
			 val d v .&. 0xFF
	f i a d (l@("ADD.B":('(':'I':')':',':x):xs):ls)
		| last x == ';' = testMSB a i x' $
			line a (op + jo xs) (Just $ jc d xs) l $ f i (a+2) d ls
		| otherwise = testMSB a i x' $ line a op Nothing l $ f i (a+1) d ls
	 where op = 0x8000 + yx x' ; x' = loc d (if last x == ';' then init x else x)
	f i a d (l@("ADD.N":('(':'I':')':',':x):xs):ls)
		| last x == ';' = testMSB a i x' $
			line a (op + jo xs) (Just $ jc d xs) l $ f i (a+2) d ls
		| otherwise = testMSB a i x' $ line a op Nothing l $ f i (a+1) d ls
	 where op = 0x8100 + yx x' ; x' = loc d (if last x == ';' then init x else x)
	f i a d (l@("CALL":_):ls) =
		line a (0xD000 + jo l) (Just (jc d l)) l $ f i (a+2) d ls
	f i a d (l@("CLEAR":x:_):ls) = line a op Nothing l $ f i (a+1) d ls
	 where
		(u,'.':v) = break ('.' ==) (flag d x)
		op = 0x2800 .|. yx (loc d u) .|. loc d u `shiftL` 1 .&. 0x0200 .|.
		     uh v `shiftL` 9 .&. 0x0400 .|. uh v `shiftL` 8 .&. 0x0100
	f i a d (l@("DADD":x:xs):ls)
		| last x == ';' =
			line a (op + jo xs) (Just $ jc d xs) l $ f i (a+2) d ls
		| otherwise = line a op Nothing l $ f i (a+1) d ls
	 where
		(u,',':v) = break (',' ==) (if last x == ';' then init x else x)
		t = loc d u
		op | u == "(I)" = 0xE800 .|. size (val' d v) .|. val d v .&. 0xFF
		   | head x == 'A' = 0xF800 .|. size (val' d v) .|. val d v .&. 0xFF
		   | otherwise =
			0xC800 .|. t `shiftL` 4 .&. 0x01F0 .|. t `shiftL` 7 .&. 0x1000 .|.
			 val d v .&. 0xFF
	f i a d (l@("DADD.B":('(':'I':')':',':x):xs):ls)
		| last x == ';' = testMSB a i x' $
			line a (op + jo xs) (Just $ jc d xs) l $ f i (a+2) d ls
		| otherwise = testMSB a i x' $ line a op Nothing l $ f i (a+1) d ls
	 where op = 0x8800 + yx x' ; x' = loc d (if last x == ';' then init x else x)
	f i a d (l@("DADD.N":('(':'I':')':',':x):xs):ls)
		| last x == ';' = testMSB a i x' $
			line a (op + jo xs) (Just $ jc d xs) l $ f i (a+2) d ls
		| otherwise = testMSB a i x' $ line a op Nothing l $ f i (a+1) d ls
	 where op = 0x8900 + yx x' ; x' = loc d (if last x == ';' then init x else x)
	f i a d (("DB":xs):("DB":ys):ls) =
		f i a d (("DB":g (takeWhile (not . ('#' ==) . head) xs) ys):ls)
	 where g xs (y:ys) = init xs ++ [last xs ++ "," ++ y] ++ ys
	f i a d (l@("DB":xs):ls) = g xs where
		g (x:xs) = g1 xs $ break (',' ==) x
		g1 xs (x,"") = line a (val d x * 0x100) Nothing l $ f i (a+1) d ls
		g1 (y:xs) (x,",") = g2 x xs $ break (',' ==) y
		g1 xs (x,',':y) = g2 x xs $ break (',' ==) y
		g2 x xs (y,"") = line a (val d x * 0x100 + val d y .&. 0xFF) Nothing
		 l $ f i (a+1) d ls
		g2 x (x':xs) (y,",") = g3 x y xs $ break (',' ==) x'
		g2 x xs (y,',':x') = g3 x y xs $ break (',' ==) x'
		g3 x y xs (x',"") = line a (val d x * 0x100 + val d y .&. 0xFF)
		 (Just $ val d x' * 0x100) l $ f i (a+2) d ls
		g3 x y (y':xs) (x',",") = g4 x y x' xs $ break (',' ==) y'
		g3 x y xs (x',',':y') = g4 x y x' xs $ break (',' ==) y'
		g4 x y x' xs (y',"") = line a (val d x * 0x100 + val d y .&. 0xFF)
		 (Just $ val d x' * 0x100 + val d y' .&. 0xFF) l $ f i (a+2) d ls
		g4 x y x' xs (y',",") = line a (val d x * 0x100 + val d y .&. 0xFF)
		 (Just $ val d x' * 0x100 + val d y' .&. 0xFF) l $ f i (a+2) d
		 (("DB":xs):ls) where l = ["DB",concat $ intersperse "," [x,y,x',y']]
		g4 x y x' xs (y',',':z) = g4 x y x' (z:xs) (y',",")
	f i a d (l@("DSUB.B":('(':'I':')':',':x):xs):ls)
		| last x == ';' = testMSB a i x' $
			line a (op + jo xs) (Just $ jc d xs) l $ f i (a+2) d ls
		| otherwise = testMSB a i x' $ line a op Nothing l $ f i (a+1) d ls
	 where op = 0x9800 + yx x' ; x' = loc d (if last x == ';' then init x else x)
	f i a d (l@("DSUB.N":('(':'I':')':',':x):xs):ls)
		| last x == ';' = testMSB a i x' $
			line a (op + jo xs) (Just $ jc d xs) l $ f i (a+2) d ls
		| otherwise = testMSB a i x' $ line a op Nothing l $ f i (a+1) d ls
	 where op = 0x9900 + yx x' ; x' = loc d (if last x == ';' then init x else x)
	f i a d (l@("DW":xs):ls) = g xs where
		g (x:xs) = g1 xs $ break (',' ==) x
		g1 xs (x,"") = line a (val d x) Nothing l $ f i (a+1) d ls
		g1 (y:xs) (x,",") = g2 x xs $ break (',' ==) y
		g1 xs (x,',':y) = g2 x xs $ break (',' ==) y
		g2 x xs (y,"") = line a (val d x) (Just $ val d y) l $ f i (a+2) d ls
		g2 x xs (y,",") = line a (val d x) (Just $ val d y) l $
		 f i (a+2) d (("DW":xs):ls) where l = ["DW",x ++ ',':y]
		g2 x xs (y,',':z) = g2 x (z:xs) (y,",")
	f i a d (l@("DWLE":xs):ls) = g xs where
		g (x:xs) = g1 xs $ break (',' ==) x
		g1 xs (x,"") = line a (valLE d x) Nothing l $ f i (a+1) d ls
		g1 (y:xs) (x,",") = g2 x xs $ break (',' ==) y
		g1 xs (x,',':y) = g2 x xs $ break (',' ==) y
		g2 x xs (y,"") = line a (valLE d x) (Just $ valLE d y) l $ f i (a+2) d ls
		g2 x xs (y,",") = line a (valLE d x) (Just $ valLE d y) l $
		 f i (a+2) d (("DWLE":xs):ls) where l = ["DWLE",x ++ ',':y]
		g2 x xs (y,',':z) = g2 x (z:xs) (y,",")
	f i a d (l@("IF":('(':'I':'+':')':c:x):xs):ls) =
		line a (0x6000 + cmp c + size (val' d x') + val d x') (Just (jc d xs)) l $
		 f i (a+2) d ls where x' = takeWhile (':' /=) x
	f i a d (l@("IF":('A':c:x):xs):ls) =
		line a (0x7000 + cmp c + size (val' d x') + val d x') (Just (jc d xs)) l $
		 f i (a+2) d ls where x' = takeWhile (':' /=) x
	f i a d (l@("IF":"CLEAR":x:xs):ls) = line a op (Just $ jc d xs) l $
	 f i (a+2) d ls where
		(u,'.':v) = break ('.' ==) (flag d (init x))
		op = 0xA000 .|. yx (loc d u) .|. loc d u `shiftL` 1 .&. 0x0200 .|.
		     uh v `shiftL` 9 .&. 0x0400 .|. uh v `shiftL` 8 .&. 0x0100
	f i a d (l@("IF":"SET":x:xs):ls) = line a op (Just $ jc d xs) l $
	 f i (a+2) d ls where
		(u,'.':v) = break ('.' ==) (flag d (init x))
		op = 0xA800 .|. yx (loc d u) .|. loc d u `shiftL` 1 .&. 0x0200 .|.
		     uh v `shiftL` 9 .&. 0x0400 .|. uh v `shiftL` 8 .&. 0x0100
	f i a d (l@("IF":x:xs):ls) = line a op (Just (jc d xs)) l $ f i (a+2) d ls
	 where
		(u,c:v) = break (flip elem "=≠<≥") x
		t = loc d u
		op = 0x4000 .|. cmp c .|. t `shiftL` 4 .&. 0x01F0 .|.
		 t `shiftL` 7 .&. 0x1000 .|. val d (takeWhile (':' /=) v) .&. 0xFF
	f i a d (l@("IF.B":('(':'I':')':c:x):xs):ls) = testMSB a i x' $
		line a (0xB000 + cmp c + yx x') (Just (jc d xs)) l $
		 f i (a+2) d ls where x' = loc d (takeWhile (':' /=) x)
	f i a d (l@("IF.N":('(':'I':')':c:x):xs):ls) = testMSB a i x' $
		line a (0xB100 + cmp c + yx x') (Just (jc d xs)) l $
		 f i (a+2) d ls where x' = loc d (takeWhile (':' /=) x)
	f i a d (l@("JUMP":"(104)":_):ls) = line a 0x0080 Nothing l $ f 2 (a+1) d ls
	f i a d (l@("JUMP":"(JUMP_TARGET)":_):ls) =
		line a 0x0080 Nothing l $ f 2 (a+1) d ls
	f i a d (l@("JUMP":_):ls) =
		line a (0xD000 + jo l) (Just (jc d l)) l $ f 2 (a+2) d ls
	f i a d (l@("LOAD":"(I+),BYTE":"PTR":x:_):ls) =
		line a (0x0C00 .|. val d x `shiftL` 1 .&. 0xFE)
		       (Just $ 0x0C00 .|. val d x `shiftR` 7 .&. 0xFF) l $ f i (a+2) d ls
	f i a d (l@("LOAD":"(I+),BYTE":"PTR+1":x:_):ls) =
		line a (0x0C00 .|. val d x `shiftL` 1 .&. 0xFE .|. 1)
		       (Just $ 0x0C00 .|. val d x `shiftR` 7 .&. 0xFF) l $ f i (a+2) d ls
	f i a d (l@("LOAD":"(I+),BYTE":"LPTR":x:_):ls) =
		line a (0x0C00 .|. val d x `shiftL` 1 .&. 0xFE) Nothing l $ f i (a+2) d ls
	f i a d (l@("LOAD":"(I+),BYTE":"LPTR+1":x:_):ls) =
		line a (0x0C00 .|. val d x `shiftL` 1 .&. 0xFE .|. 1) Nothing l $
			f i (a+2) d ls
	f i a d (l@("LOAD":"(I+),BYTE":"HPTR":x:_):ls) =
		line a (0x0C00 .|. val d x `shiftR` 7 .&. 0xFF) Nothing l $ f i (a+2) d ls
	f i a d (l@("LOAD":('(':'I':'+':')':',':x):_):ls) =
		line a op op' l $ f i (a+a') d ls where
		op | length (val' d x) == 4 = 0x0C00 .|. val d x .&. 0xFF
		   | otherwise = 0x0C00 + size (val' d x) + val d x .&. 0xFF
		op' | length (val' d x) == 4 = Just $ 0x0C00 .|. val d x `shiftR` 8 .&. 0xFF
		    | otherwise = Nothing
		a' = if length (val' d x) == 4 then 2 else 1
	f i a d (l@("LOAD":('A':',':x):_):ls) =
		line a (0x1C00 + size (val' d x) + val d x .&. 0xFF) Nothing l $
		 f i (a+1) d ls
	f i a d (l@("LOAD":('I':',':x):xs):ls)
		| last x == ';' = line a (0x0A00 + reg d (init x)) (Just $ jc d xs) l $
		 f (reg d (init x) `shiftR` 8) (a+2) d ls
		| otherwise = line a (0x0800 + reg d x) Nothing l $
		 f (reg d x `shiftR` 8) (a+1) d ls
	f i a d (l@("LOAD":x:_):ls) = line a op Nothing l $ f i (a+1) d ls where
		(u,',':v) = break (',' ==) x
		t = loc d u
		op = 0x0E00 .|. t `shiftL` 4 .&. 0x01F0 .|. t `shiftL` 7 .&. 0x1000 .|.
		 val d v .&. 0xFF
	f i a d (l@("LOAD.B":('A':',':x):_):ls) | x /= "(I)" =
		line a op Nothing l $ f i (a+1) d ls
	 where op = 0x1000 .|. yx (loc d x) .|. loc d x `shiftL` 1 .&. 0x0200
	f i a d (l@("LOAD.B":('(':'I':')':',':x):_):ls) = testMSB a i x' $
		line a op Nothing l $ f i (a+1) d ls
	 where op = 0x0400 .|. yx x' ; x' = loc d x
	f i a d (l@("LOAD.B":x:_):ls)
		| last x == 'A' = line a op Nothing l $ f i (a+1) d ls
		| otherwise = testMSB a i x'' $ line a op Nothing l $ f i (a+1) d ls
	 where
		op | last x == 'A' = 0x1800 .|. yx x' .|. x' `shiftL` 1 .&. 0x0200
		   | otherwise = 0x0200 .|. yx x''
		x' = loc d $ init $ init x
		x'' = loc d $ init $ init $ init $ init x
	f i a d (l@("LOAD.N":('A':',':x):_):ls) | x /= "(I)" =
		line a op Nothing l $ f i (a+1) d ls
	 where op = 0x1100 .|. yx (loc d x) .|. loc d x `shiftL` 1 .&. 0x0200
	f i a d (l@("LOAD.N":('(':'I':')':',':x):_):ls) = testMSB a i x' $
		line a op Nothing l $ f i (a+1) d ls
	 where op = 0x0500 .|. yx x' ; x' = loc d x
	f i a d (l@("LOAD.N":x:_):ls)
		| last x == 'A' = line a op Nothing l $ f i (a+1) d ls
		| otherwise = testMSB a i x'' $ line a op Nothing l $ f i (a+1) d ls
	 where
		op | last x == 'A' = 0x1900 .|. yx x' .|. x' `shiftL` 1 .&. 0x0200
		   | otherwise = 0x0300 .|. yx x''
		x' = loc d $ init $ init x
		x'' = loc d $ init $ init $ init $ init x
	f i a d (l@("NOP":_):ls) = line a 0xD000 Nothing l $ f i (a+1) d ls
	f i a d (l@("NOT.B":x:_):ls) = line a op Nothing l $ f i (a+1) d ls
	 where op = 0x3800 .|. yx (loc d x) .|. loc d x `shiftL` 1 .&. 0x0200
	f i a d (l@("NOT.N":x:_):ls) = line a op Nothing l $ f i (a+1) d ls
	 where op = 0x3900 .|. yx (loc d x) .|. loc d x `shiftL` 1 .&. 0x0200
	f i a d (l@("OR.B":('(':'I':')':',':x):xs):ls) = testMSB a i (loc d x) $
		line a (0x3E00 + yx (loc d x)) Nothing l $ f i (a+1) d ls
	f i a d (l@("OR.N":('(':'I':')':',':x):xs):ls) = testMSB a i (loc d x) $
		line a (0x3F00 + yx (loc d x)) Nothing l $ f i (a+1) d ls
	f i a d (l@("READD":_):ls) = line a 0x0005 Nothing l $ f i (a+1) d ls
	f i a d (l@("READD.B":_):ls) = line a 0x0005 Nothing l $ f i (a+1) d ls
	f i a d (l@("READD.N":_):ls) = line a 0x0105 Nothing l $ f i (a+1) d ls
	f i a d (l@("READU":_):ls) = line a 0x0004 Nothing l $ f i (a+1) d ls
	f i a d (l@("READU.B":_):ls) = line a 0x0004 Nothing l $ f i (a+1) d ls
	f i a d (l@("READU.N":_):ls) = line a 0x0104 Nothing l $ f i (a+1) d ls
	f i a d (l@("REP":x:_):ls) =
		line a (0x0F00 + uh x - 1) Nothing l $ f i (a+1) d ls
	f i a d (l@("RET":_):ls) = line a 0x0040 Nothing l $ f i (a+1) d ls
	f i a d (l@("SCANKEYS":_):ls) = line a 0x0020 Nothing l $ f i (a+1) d ls
	f i a d (l@("SET":x:_):ls) = line a op Nothing l $ f i (a+1) d ls where
		(u,'.':v) = break ('.' ==) (flag d x)
		op = 0x3000 .|. yx (loc d u) .|. loc d u `shiftL` 1 .&. 0x0200 .|.
		     uh v `shiftL` 9 .&. 0x0400 .|. uh v `shiftL` 8 .&. 0x0100
	f i a d (l@("SHR":"A":_):ls) = line a 0x0018 Nothing l $ f i (a+1) d ls
	f i a d (l@("SUB.B":('(':'I':')':',':x):xs):ls)
		| last x == ';' = testMSB a i x' $
			line a (op + jo xs) (Just $ jc d xs) l $ f i (a+2) d ls
		| otherwise = testMSB a i x' $ line a op Nothing l $ f i (a+1) d ls
	 where op = 0x9000 + yx x' ; x' = loc d (if last x == ';' then init x else x)
	f i a d (l@("SUB.N":('(':'I':')':',':x):xs):ls)
		| last x == ';' = testMSB a i x' $
			line a (op + jo xs) (Just $ jc d xs) l $ f i (a+2) d ls
		| otherwise = testMSB a i x' $ line a op Nothing l $ f i (a+1) d ls
	 where op = 0x9100 + yx x' ; x' = loc d (if last x == ';' then init x else x)
	f i a d (l@("TOGGLE":x:_):ls) = line a op Nothing l $ f i (a+1) d ls
	 where
		(u,'.':v) = break ('.' ==) (flag d x)
		op = 0x2000 .|. yx (loc d u) .|. loc d u `shiftL` 1 .&. 0x0200 .|.
		     uh v `shiftL` 9 .&. 0x0400 .|. uh v `shiftL` 8 .&. 0x0100
	f i a d (l@("WRITED":_):ls) = line a 0x0007 Nothing l $ f i (a+1) d ls
	f i a d (l@("WRITED.B":_):ls) = line a 0x0007 Nothing l $ f i (a+1) d ls
	f i a d (l@("WRITED.N":_):ls) = line a 0x0107 Nothing l $ f i (a+1) d ls
	f i a d (l@("WRITEU":_):ls) = line a 0x0006 Nothing l $ f i (a+1) d ls
	f i a d (l@("WRITEU.B":_):ls) = line a 0x0006 Nothing l $ f i (a+1) d ls
	f i a d (l@("WRITEU.N":_):ls) = line a 0x0106 Nothing l $ f i (a+1) d ls
	f i a d (l@("XCHG.B":('A':',':x):_):ls) = line a op Nothing l $ f i (a+1) d ls
	 where op = 0x1400 .|. yx (loc d x) .|. loc d x `shiftL` 1 .&. 0x0200
	f i a d (l@("XCHG.B":('(':'I':')':',':x):_):ls) = testMSB a i (loc d x) $
		line a (0x0600 .|. yx (loc d x)) Nothing l $ f i (a+1) d ls
	f i a d (l@("XCHG.N":('A':',':x):_):ls) = line a op Nothing l $ f i (a+1) d ls
	 where op = 0x1500 .|. yx (loc d x) .|. loc d x `shiftL` 1 .&. 0x0200
	f i a d (l@("XCHG.N":('(':'I':')':',':x):_):ls) = testMSB a i (loc d x) $
		line a (0x0700 .|. yx (loc d x)) Nothing l $ f i (a+1) d ls
	f i a d (l@("XOR.B":('(':'I':')':',':x):xs):ls) = testMSB a i (loc d x) $
		line a (0x3C00 + yx (loc d x)) Nothing l $ f i (a+1) d ls
	f i a d (l@("XOR.N":('(':'I':')':',':x):xs):ls) = testMSB a i (loc d x) $
		line a (0x3D00 + yx (loc d x)) Nothing l $ f i (a+1) d ls
	f _ (a,_) _ ((l:_):_) = error $ h a ++ ": unknown instruction: " ++ l
