with Interfaces;                        use Interfaces;
with UnZ_Glob;                          use UnZ_Glob;
with UnZ_IO;                            use UnZ_IO;

package body UnZ_Copy is

-- Note from Pascal source:
-- C code by info-zip group, translated to pascal by Christian Ghisler
-- based on unz51g.zip

-- ************************* copy stored file *****************************

procedure copystored is
  readin : unsigned_32;

BEGIN
  WHILE  reachedsize < compsize  LOOP

    readin := compsize - reachedsize;

    IF  readin > wsize THEN readin := wsize; END IF;

    BEGIN
      for i in 0 .. readin-1 loop
        Byte_IO.Read( infile, slide( Natural(i) ) ); -- Use slide as buffer
      end loop;
    EXCEPTION
      when others=> raise Read_Error;
    END;

    BEGIN
      Flush ( Natural(readin) );  -- Flush output takes care of CRC too
    EXCEPTION
      when others=> raise Write_error;
    END;
    reachedsize:= reachedsize + readin;
  END LOOP;

END copystored;

end UnZ_Copy;
