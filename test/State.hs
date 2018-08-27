module State where

-- | Represent a file handle as a zipper, so we can wind it forward and
--   backwards, and output as a (reversed) string.
type ReadM = State ((String, String), String)

readImp :: J.Imp () ReadM
readImp = J.I {
    J.getchar   = \() -> get >>= (setReturn . getchar'),
    J.lookahead = \() -> get >>= (setReturn . lookahead' . snd),
    J.seek      = seek
  }

setReturn (Just  s, x) = put s >> pure x
setReturn (Nothing, x) =          pure x

getchar' (_ , ""  ) = (Nothing        , Nothing)
getchar' (xs, y:ys) = (Just (y:xs, ys), Just y )

--lookahead :: () -> ReadM (Maybe Char)
lookahead' post = (Nothing, case post of
                              ""  -> Nothing
                              c:_ -> Just c)

seek :: () -> SeekMode -> Integer -> ReadM ()
seek () mode n = do ((pre, post), out) <- get
                    put (case mode of
                          AbsoluteSeek -> (absStep n pre post, out)
                          RelativeSeek -> (relStep n pre post, out)
                          SeekFromEnd  -> error "Shouldn't SeekFromEnd")

absStep n pre post = let whole = reverse pre ++ post
                         n'    = fromIntegral n
                      in (reverse (take n' whole), drop n' whole)

relStep 0 pre post = (pre, post)
relStep n pre post = case (n > 0, pre, post) of
                       (True , _   , c:cs) -> relStep (n-1) (c:pre) cs
                       (True , _   , _   ) -> error "Seeked too far"
                       (False, c:cs, _   ) -> relStep (n+1) cs (c:post)
                       (False, _   , _   ) -> error "Went back too far"
