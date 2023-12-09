--------------------------------------------------------------
-- Copyright(c) 2018 ADTRAN, Inc. All Rights Reserved
--------------------------------------------------------------
--
-- $Revision: $
--------------------------------------------------------------

library ieee;
   use ieee.std_logic_1164.all;

entity comp is
   generic(
      arst0_srst1  : std_logic := '0';
      rst_act_lev  : std_logic := '0';
      srst_loc_buf : boolean  := false
   );
   port(
      clk    : in std_logic;
      arst_g : in std_logic;
      srst_g : in std_logic;

      -- Add more here
   );
end comp;

architecture rtl of comp is
   --------------------------------------------------
   -- Components
   --------------------------------------------------

   --------------------------------------------------
   -- Constants 
   --------------------------------------------------

   --------------------------------------------------
   -- Types
   --------------------------------------------------

   type state_t is (
      idle
   );

   --------------------------------------------------
   -- Signals
   --------------------------------------------------
   signal srst_i : std_logic;
begin

   srst_loc_buf_true_g : if (srst_loc_buf = true) generate
      component gl_dff port (clk, d : in std_logic; q : out std_logic); end component;
   begin
      srst_loc_ff_g : gl_dff port map (clk => clk, d => srst_g, q => srst_i);
   end generate;
   srst_loc_buf_false_g : if (srst_loc_buf = false) generate
   begin
      srst_i <= srst_g;
   end generate;

   p : process(clk, arst_g)
      procedure reset_signals is
      begin
         -- Add resets here
         state <= idle;
      end procedure;
   begin
      if(arst_g = rst_act_lev and arst0_srst1 = '0') then
         reset_signals;
      elsif(clk'event and clk = '1') then
         if(srst_i = rst_act_lev and arst0_srst1 = '1') then
            reset_signals;
         else
            -- Add code here

            case(state) is
               when idle =>
               when others =>
                  state <= idle;
            end case;

         end if; -- srst_i
      end if; -- arst_g/clk
   end process;

end rtl;

