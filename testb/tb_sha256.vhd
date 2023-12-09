-- Self checking unit testbench for sha256 computation unit.
-- Uses golden vectors provided by NIST.
-- https://csrc.nist.gov/projects/cryptographic-standards-and-guidelines/example-values
--------------------------------------------------------------

library ieee;
   use ieee.std_logic_1164.all;

entity tb is
end tb;

architecture testbench of tb is

   component sha256 is
      generic(
         rst_act_lev  : std_logic := '0'
      );
      port(
         clk     : in std_logic;
         arst_g  : in std_logic;
   
         -- Message interface
         Mi_rd   : out std_logic;
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
   constant clk_period  : time := 10 ns;
   constant rst_act_lev : std_logic := '1';

   -- One block message with golden vector
   --constant M           : std_logic_vector(511 downto 0) := x"61626300000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000";
   --constant LEN         : std_logic_vector(63 downto 0):= x"0000000000000018";
   --constant NUM_WORDS   : integer := 1;
   --constant GV          : std_logic_vector(255 downto 0) := x"BA7816BF_8F01CFEA_414140DE_5DAE2223_B00361A3_96177A9C_B410FF61_F20015AD";

   -- Two block message with golden vector
   constant M           : std_logic_vector(511 downto 0) := x"61626364_62636465_63646566_64656667_65666768_66676869_6768696A_68696A6B_696A6B6C_6A6B6C6D_6B6C6D6E_6C6D6E6F_6D6E6F70_6E6F7071_00000000_00000000";
   constant LEN         : std_logic_vector(63 downto 0):= x"00000000000001C0";
   constant NUM_WORDS   : integer := 15;
   constant GV          : std_logic_vector(255 downto 0) := x"248D6A61_D20638B8_E5C02693_0C3E6039_A33CE459_64FF2167_F6ECEDD4_19DB06C1";

   --------------------------------------------------
   -- Signals
   --------------------------------------------------
   signal clk     : std_logic;
   signal arst_g  : std_logic := rst_act_lev;
   signal Mi_rd   : std_logic;
   signal Mi      : std_logic_vector(511 downto 0);
   signal M_len   : std_logic_vector(63 downto 0);
   signal M_som   : std_logic := '0';
   signal M_eom   : std_logic := '0';
   signal rd_rdy  : std_logic;
   signal dgt_vld : std_logic;
   signal dgt     : std_logic_vector(255 downto 0);

   signal wcnt    : integer := 0;

begin

   clk_p : process 
   begin
      clk <= '1', '0' after clk_period/2;
      wait for clk_period;
   end process;

   main : process
   begin
      
      wait for 50 ns;
      arst_g <= not rst_act_lev;

      wait until clk'event and clk='1';
      M_som  <= '1';
      M_eom  <= '1';
      M_len  <= LEN;
      Mi     <= M;
      rd_rdy <= '1';

      wait until clk'event and clk='1' and Mi_rd='1';
      M_som  <= '0';
      M_eom  <= '0';
      rd_rdy <= '0';

      wait until clk'event and clk='1' and dgt_vld='1';
      if dgt = GV then
         assert false report "Pass" severity note;
      else 
         assert false report "Fail" severity note;
      end if;

      wait;

   end process;

   hash : sha256 
      generic map(
         rst_act_lev  => rst_act_lev
      )
      port map(
         clk     => clk,    
         arst_g  => arst_g, 
                           
         -- Message interface
         Mi_rd  => Mi_rd, 
         Mi      => Mi,     
         M_len   => M_len,  
         M_som   => M_som,  
         M_eom   => M_eom,  
         rd_rdy  => rd_rdy,    
                           
         -- Digest interface
         dgt_vld => dgt_vld,
         dgt     => dgt    
      );


end testbench;

