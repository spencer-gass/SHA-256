-- Top level wrapper for SHA256 test build
-- This wrapper allows for test build for estimating 
-- utilization and isn't intended to be functional.
--------------------------------------------------------------

library ieee;
   use ieee.std_logic_1164.all;

entity sha256_top is
   port(
      clk     : in  std_logic;
      arst_g  : in  std_logic;

      msg     : in  std_logic;
      msg_rd  : out std_logic;
      som     : in  std_logic;
      eom     : in  std_logic;
      rdy     : in  std_logic;

      dgt     : out std_logic
   );
end sha256_top;

architecture rtl of sha256_top is
   --------------------------------------------------
   -- Components
   --------------------------------------------------
   component sha256 is
      generic(
         rst_act_lev  : std_logic := '0'
      );
      port(
         clk     : in std_logic;
         arst_g  : in std_logic;
   
         -- Message interface
         Mi_rd   : out  std_logic;
         Mi      : in  std_logic_vector(511 downto 0);
         M_len   : in  std_logic_vector(63 downto 0);
         M_som   : in  std_logic;
         M_eom   : in  std_logic;
         rd_rdy  : in  std_logic;
        
         -- Digest interface
         dgt_vld : out std_logic;
         dgt     : out std_logic_vector(255 downto 0)
      );
   end component;

   --------------------------------------------------
   -- Constants 
   --------------------------------------------------

   constant rst_act_lev : std_logic := '1';

   --------------------------------------------------
   -- Types
   --------------------------------------------------
   
   signal msg_i       : std_logic_vector(511 downto 0);
   signal len_i       : std_logic_vector(63 downto 0);

   signal dgt_vld     : std_logic;
   signal dgt_i       : std_logic_vector(255 downto 0);
   signal dgt_shift   : std_logic_vector(255 downto 0);
 
   --------------------------------------------------
   -- Signals
   --------------------------------------------------
begin

   p : process(clk, arst_g)
      procedure reset_signals is
      begin
         null;
      end procedure;
   begin
      if(arst_g = rst_act_lev) then
         reset_signals;
      elsif(clk'event and clk = '1') then

      msg_i <= msg_i(510 downto 0) & msg;

      len_i <= len_i(62 downto 0) & msg_i(511);

      dgt <= dgt_shift(255);

      if dgt_vld='1' then
         dgt_shift <= dgt_i;
      else
         dgt_shift <= dgt_shift(254 downto 0) & dgt_shift(255);
      end if;

      end if; -- arst_g/clk
   end process;

   hash : sha256 
      generic map(
         rst_act_lev => rst_act_lev
      )
      port map(
         clk     => clk,   
         arst_g  => arst_g,
   
         -- Message interface
         Mi_rd   => msg_rd,
         Mi      => msg_i,
         M_len   => len_i,
         M_som   => som,
         M_eom   => eom,
         rd_rdy  => rdy,
        
         -- Digest interface
         dgt_vld => dgt_vld,
         dgt     => dgt_i    
      );


end rtl;

