-- ***************** CRC Checking **************************
-- ////// by the African Chief  ////////////////////////////

with Interfaces;                        use Interfaces;
with UnZ_Glob;                          use UnZ_Glob;

package UnZ_CRC is

  PROCEDURE InitCRC32( CRC: in out unsigned_32 );
  FUNCTION  FinalCRC32( CRC: unsigned_32 ) return unsigned_32;

  PROCEDURE UpdateCRC( s: IO_buf );

end UnZ_CRC;
