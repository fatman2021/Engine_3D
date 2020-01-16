--  ___  ____  ____  ____  ________  ___   ______       ______     ___
--  |.|  |../  |...\ |../ /___..._/  |.|   |.___.\     /. __ .\  __|.|   ____
--  |.|  |.|   |.|\.\|.|     /../    |.|   |.____/     |.|__|.| /....|  __\..\
--  |.|__|.|   |.| \...|   _/../___  |.|   |.|    ===  |..__..||. = .| | = ..|
--  |______|  /__|  \__|  /_______/  |_|  /__|        /__|  |_| \__\_|  \__\_|

-- UnZip.Streams
----------------
-- Unzips files into a stream

-- Legal note:

--  Copyright (c) 2000 Gautier de Montmollin
--  CH-2000 Neuchatel
--  SWITZERLAND
--  Permission granted to use this library, without any warranty, for any
--  purpose, provided this copyright note remains attached and unmodified.

with Unzip;
with Ada.Streams;                       use Ada.Streams;
with Ada.Streams.Stream_IO;
with Ada.IO_Exceptions;

package Unzip.Streams is

   subtype Stream_Access is Ada.Streams.Stream_IO.Stream_Access;

   type Zipped_File_Type is private;

   procedure Open
     (File         : in out Zipped_File_Type; -- File-in-archive handle
      Archive_Name : in String;               -- Name of archive file
      Name         : in String                -- Name of zipped entry
     );

   procedure Open
     (File         : in out Zipped_File_Type; -- File-in-archive handle
      Archive_Info : in zip_info;             -- (see Unzip.Load_zip_info)
      Name         : in String                -- Name of zipped entry
     );

   procedure Close (File : in out Zipped_File_Type);

   function Is_Open     (File : in Zipped_File_Type) return Boolean;
   function End_Of_File (File : in Zipped_File_Type) return Boolean;

   function Stream (File : Zipped_File_Type) return Stream_Access;

   Use_Error    : exception renames Ada.IO_Exceptions.Use_Error;
   End_Error    : exception renames Ada.IO_Exceptions.End_Error;

private

   type UZS_state is (
      uninitialized,
      data_uncompressed, -- In that model, all data is unzipped in one
                         --   time, into memory. If you have a smarter
                         --   idea (small buffer with tasking, write me!)
      end_of_zip         -- We have reached the end, not yet closed
     );

   type p_String is access String;
   type p_Stream_Element_Array is access Stream_Element_Array;

   type Unzip_Stream_Type is new Root_Stream_Type with record
      state       : UZS_state:= uninitialized;
      archive_info: Unzip.zip_info; -- archive info (.zip file, directory)
      delete_info_on_closing : Boolean:= False;
      file_name    : p_String; -- name of zipped file to unzip from archive
      uncompressed : p_Stream_Element_Array; -- whole uncompressed data
      index        : Stream_Element_Offset;
   end record;


   procedure Read
     (Stream : in out Unzip_Stream_Type;
      Item   :    out Stream_Element_Array;
      Last   :    out Stream_Element_Offset);

   procedure Write
     (Stream : in out Unzip_Stream_Type;
      Item   : in     Stream_Element_Array);

   type Zipped_File_Type is access Unzip_Stream_Type;

end Unzip.Streams;
