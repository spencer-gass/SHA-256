-- 256 bit secure hash algorithm (SHA256) as defined in
-- FIPS PUB 180-4: Secure Hash Standard (SHS)
--------------------------------------------------------------

library ieee;
   use ieee.std_logic_1164.all;
   use ieee.std_logic_arith.all;
   use ieee.std_logic_unsigned.all;

entity sha256 is
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
end sha256;

architecture rtl of sha256 is
   --------------------------------------------------
   -- Functions
   --------------------------------------------------

   -- Operations (section 2.2.2 of SHS) --

   function rotr (x : std_logic_vector(31 downto 0);
                  n : integer)
                  return std_logic_vector is
   begin
      if (n mod 32) = 0 then
         return x;
      else
         return x((n mod 32)-1 downto 0) & x(31 downto (n mod 32));
      end if;
   end function;

   function shr (x : std_logic_vector(31 downto 0);
                 n : integer)
                 return std_logic_vector is
      variable s : std_logic_vector(31 downto 0);
   begin
      if n = 0 then
         return x;
      elsif n > 31 then 
         return x"00000000";
      else -- 1 <= n <= 31
         s(31 downto 32-n) := (others => '0');
         s(31-n downto 0)  := x(31 downto n);
         return s;
      end if;
   end function;

   -- Functions (section 4.1.2 of SHS) --

   function Ch (x : std_logic_vector(31 downto 0);
                y : std_logic_vector(31 downto 0);
                z : std_logic_vector(31 downto 0)) 
                return std_logic_vector is
   begin
      return (x and y) xor (not x and z);
   end function;

   function Maj (x : std_logic_vector(31 downto 0);
                 y : std_logic_vector(31 downto 0);
                 z : std_logic_vector(31 downto 0)) 
                 return std_logic_vector is
   begin
      return (x and y) xor (x and z) xor (y and z);
   end function;

   function uc_sigma0 (x : std_logic_vector(31 downto 0)) 
                       return std_logic_vector is
   begin
      return rotr(x,2) xor rotr(x,13) xor rotr(x,22);
   end function;

   function uc_sigma1 (x : std_logic_vector(31 downto 0)) 
                       return std_logic_vector is
   begin
      return rotr(x,6) xor rotr(x,11) xor rotr(x,25);
   end function;

   function lc_sigma0 (x : std_logic_vector(31 downto 0)) 
                       return std_logic_vector is
   begin
      return rotr(x,7) xor rotr(x,18) xor shr(x,3);
   end function;

   function lc_sigma1 (x : std_logic_vector(31 downto 0)) 
                       return std_logic_vector is
   begin
      return rotr(x,17) xor rotr(x,19) xor shr(x,10);
   end function;

   --------------------------------------------------d
   -- Types
   --------------------------------------------------

   type wd_vec is array (integer range <>) of std_logic_vector(31 downto 0);

   --------------------------------------------------
   -- Constants 
   --------------------------------------------------
   
   -- First 32 bits of the fractional part of the cube roots 
   -- of the first 64 prime numbers (section 4.2.2 SHS):
   constant K : wd_vec(0 to 63) := (x"428a2f98", x"71374491", x"b5c0fbcf", x"e9b5dba5", x"3956c25b", x"59f111f1", x"923f82a4", x"ab1c5ed5",
                                    x"d807aa98", x"12835b01", x"243185be", x"550c7dc3", x"72be5d74", x"80deb1fe", x"9bdc06a7", x"c19bf174",
                                    x"e49b69c1", x"efbe4786", x"0fc19dc6", x"240ca1cc", x"2de92c6f", x"4a7484aa", x"5cb0a9dc", x"76f988da",
                                    x"983e5152", x"a831c66d", x"b00327c8", x"bf597fc7", x"c6e00bf3", x"d5a79147", x"06ca6351", x"14292967",
                                    x"27b70a85", x"2e1b2138", x"4d2c6dfc", x"53380d13", x"650a7354", x"766a0abb", x"81c2c92e", x"92722c85",
                                    x"a2bfe8a1", x"a81a664b", x"c24b8b70", x"c76c51a3", x"d192e819", x"d6990624", x"f40e3585", x"106aa070",
                                    x"19a4c116", x"1e376c08", x"2748774c", x"34b0bcb5", x"391c0cb3", x"4ed8aa4a", x"5b9cca4f", x"682e6ff3",
                                    x"748f82ee", x"78a5636f", x"84c87814", x"8cc70208", x"90befffa", x"a4506ceb", x"bef9a3f7", x"c67178f2");

   -- Initial hash values (section 5.3.3 SHS):
   constant Hj0 : wd_vec(0 to 7) := (x"6A09E667", 
                                     x"BB67AE85",
                                     x"3C6EF372",
                                     x"A54FF53A",
                                     x"510E527F",
                                     x"9B05688C",
                                     x"1F83D9AB",
                                     x"5BE0CD19");

   --------------------------------------------------
   -- Signals
   --------------------------------------------------
   
   signal Mi_rd_vld        : std_logic := '0';
   signal d_cnt            : integer range 0 to 65 := 65;

   signal Mi_pad_vld       : std_logic := '0';
   signal M_pad_som        : std_logic := '0';
   signal M_pad_eom        : std_logic := '0';
   signal Mi_pad           : std_logic_vector(511 downto 0);
   signal M_len_mod512     : integer;
   signal M_len_mod512_d   : integer;
   signal add_blk          : std_logic := '0';

   signal Mi_parse_vld     : std_logic := '0';
   signal M_parse_som      : std_logic := '0';
   signal M_parse_eom      : std_logic := '0';
   signal Mj               : wd_vec(0 to 15);

   signal Mi_calc1_vld     : std_logic := '0';
   signal M_calc1_som      : std_logic := '0';
   signal M_calc1_eom      : std_logic := '0';
   signal calc1_cnt        : integer range 15 to 63 := 15;
   signal lc_sigma1_Wtm2   : std_logic_vector(31 downto 0);
   signal lc_sigma0_Wtm15  : std_logic_vector(31 downto 0);
   signal Wtm7             : std_logic_vector(31 downto 0);
   signal Wtm16            : std_logic_vector(31 downto 0);
   signal W                : wd_vec(0 to 63);
   type calc1_state_t is (idle, pre_calc, calc);
   signal calc1_state      : calc1_state_t;
   
   signal Mi_calc2_vld     : std_logic := '0';
   signal M_calc2_som      : std_logic := '0';
   signal M_calc2_eom      : std_logic := '0';
   signal calc2_cnt        : integer range 0 to 63 := 0;
   signal M_calc1_eom_hold : std_logic := '0';
   signal a                : std_logic_vector(31 downto 0);
   signal b                : std_logic_vector(31 downto 0);
   signal c                : std_logic_vector(31 downto 0);
   signal d                : std_logic_vector(31 downto 0);
   signal e                : std_logic_vector(31 downto 0);
   signal f                : std_logic_vector(31 downto 0);
   signal g                : std_logic_vector(31 downto 0);
   signal h                : std_logic_vector(31 downto 0);
   signal Hj               : wd_vec(0 to 7);
   signal K_reg            : std_logic_vector(31 downto 0);
   signal W_reg            : std_logic_vector(31 downto 0);
   signal h_plus_K_plus_W  : std_logic_vector(31 downto 0);
   type calc2_state_t is (idle, iter, add);
   signal calc2_state      : calc2_state_t;

begin

   p : process(clk, arst_g)

      procedure reset_signals is
      begin
         -- Only reset the signals at the beginning of the pipeline
         -- This minimizes reset routing and number of unique control sets 
         d_cnt     <= 65;
         Mi_rd_vld <= '0';
      end procedure;

      variable T1   : std_logic_vector(31 downto 0);
      variable T2   : std_logic_vector(31 downto 0);

   begin
      if(arst_g = rst_act_lev) then
         reset_signals;
      elsif(clk'event and clk = '1') then

         -- Read a block from a message buffer every 66 cycles
         Mi_rd_vld  <= '0';
         if (rd_rdy='1' or add_blk='1') and d_cnt=65 then
            d_cnt       <= 0;
            Mi_rd_vld   <= '1';
         elsif d_cnt < 65 then
            d_cnt <= d_cnt + 1;
         end if; 

         -- Pad (section 5.1.1 of SHS)
         -- Append a 1b'1' to the end of the messege.
         -- Pad with zeros until message bit length plus one plus zeros equals 448 (i.e. 512-64).
         -- Append 64 bit message bit length field. 
         Mi_pad_vld     <= Mi_rd_vld;
         Mi_pad         <= (others=>'0');
         M_len_mod512_d <= M_len_mod512;
         M_pad_som      <= M_som;
         M_pad_eom      <= '0';

         if Mi_rd_vld='1' and add_blk='1'then -- add an extra block to make room for length field  
            add_blk        <= '0';
            M_pad_eom <= '1';
            if M_len_mod512_d = 0 then
               Mi_pad(511)           <= '1';
               Mi_pad(63  downto 0 ) <= M_len;
            else
               Mi_pad(63  downto 0 ) <= M_len;
            end if;
         elsif Mi_rd_vld='1' and M_eom='1' then
            if M_len_mod512 = 0 then -- all bits are messege bits
               add_blk <= '1';
               Mi_pad  <= Mi;
            elsif M_len_mod512 > 447 then -- enough room to append a '1'
               add_blk <= '1';
               Mi_pad(511 downto 512-M_len_mod512) <= Mi(511 downto 512-M_len_mod512);
               Mi_pad(511-M_len_mod512) <= '1';
            else  -- room to append a '1' and the bit length
               M_pad_eom <= '1';
               Mi_pad(511 downto 512-M_len_mod512) <= Mi(511 downto 512-M_len_mod512);
               Mi_pad(511-M_len_mod512) <= '1';
               Mi_pad(63 downto 0) <= M_len;
            end if;
         elsif Mi_rd_vld='1' then 
            Mi_pad <= Mi;
         end if;

         -- Parse (section 5.2.1 SHS)
         -- Split the 512 bit block into 16 32bit words.
         Mi_parse_vld <= Mi_pad_vld;
         M_parse_som  <= M_pad_som; 
         M_parse_eom  <= M_pad_eom;
         for i in 0 to 15 loop
            Mj(i) <= Mi_pad(511-32*i downto 480-32*i);
         end loop;

         -- Compute Digest (section 6.2.2 SHS)
         -- stage1 calculate Ws
         Mi_calc1_vld <= Mi_parse_vld;
         M_calc1_som  <= M_parse_som; 
         M_calc1_eom  <= M_parse_eom; 

         case calc1_state is
            when idle =>
               if Mi_parse_vld='1' then
                  W(0 to 15)  <= Mj;
                  calc1_cnt   <= 15;
                  calc1_state <= pre_calc;
               end if;
            when pre_calc =>
               lc_sigma1_Wtm2  <= lc_sigma1(W(calc1_cnt+1-2));     
               lc_sigma0_Wtm15 <= lc_sigma0(W(calc1_cnt+1-15));
               Wtm7        <= W(calc1_cnt-7+1);
               Wtm16       <= W(calc1_cnt-16+1);
               calc1_cnt   <= calc1_cnt + 1;
               calc1_state <= calc;
            when calc =>
               -- W(calc1_cnt) <= lc_sigma1(W(calc1_cnt-2)) + W(calc1_cnt-7) + lc_sigma0(W(calc1_cnt-15)) + W(calc1_cnt-16);
               W(calc1_cnt)    <= lc_sigma1_Wtm2 + Wtm7 + lc_sigma0_Wtm15 + Wtm16;
               lc_sigma1_Wtm2  <= lc_sigma1(W(calc1_cnt+1-2));     
               lc_sigma0_Wtm15 <= lc_sigma0(W(calc1_cnt+1-15));
               Wtm7            <= W(calc1_cnt-7+1);
               Wtm16           <= W(calc1_cnt-16+1);
               if calc1_cnt = 63 then
                  calc1_state <= idle;
               else
                  calc1_cnt <= calc1_cnt + 1;
               end if;
            when others =>
               calc1_state <= idle;
         end case;

         -- stage2 calculate Hs
         Mi_calc2_vld <= '0';
         M_calc2_eom  <= '0';

         case calc2_state is
            when idle =>
               if Mi_calc1_vld='1' then 
                  calc2_cnt <= 0;
                  M_calc1_eom_hold <= M_calc1_eom;
                  K_reg <= K(1);
                  W_reg <= W(1);
                  calc2_state <= iter;
                  if M_calc1_som='1' then
                     Hj(0) <= Hj0(0);
                     Hj(1) <= Hj0(1);
                     Hj(2) <= Hj0(2);
                     Hj(3) <= Hj0(3);
                     Hj(4) <= Hj0(4);
                     Hj(5) <= Hj0(5);
                     Hj(6) <= Hj0(6);
                     Hj(7) <= Hj0(7);
                     a     <= Hj0(0); 
                     b     <= Hj0(1); 
                     c     <= Hj0(2); 
                     d     <= Hj0(3); 
                     e     <= Hj0(4); 
                     f     <= Hj0(5); 
                     g     <= Hj0(6); 
                     h     <= Hj0(7); 
                     h_plus_K_plus_W <= Hj0(7) + K(0) + W(0);
                  else
                     a <= Hj(0); 
                     b <= Hj(1); 
                     c <= Hj(2); 
                     d <= Hj(3); 
                     e <= Hj(4); 
                     f <= Hj(5); 
                     g <= Hj(6); 
                     h <= Hj(7); 
                     h_plus_K_plus_W <= Hj(7) + K(0) + W(0);
                  end if;
               end if;
            when iter =>
               -- T1 := h + uc_sigma1(e) + Ch(e,f,g) + K(calc2_cnt) + W(calc2_cnt);
               T1 := h_plus_K_plus_W + uc_sigma1(e) + Ch(e,f,g);
               T2 := uc_sigma0(a) + Maj(a,b,c);
               h  <= g;
               g  <= f;
               f  <= e;
               e  <= d + T1;
               d  <= c;
               c  <= b;
               b  <= a;
               a  <= T1 + T2;
               h_plus_K_plus_W <= g + K_reg + W_reg;
               K_reg <= K((calc2_cnt+2) mod 64);
               W_reg <= W((calc2_cnt+2) mod 64);
               if calc2_cnt = 63 then
                  calc2_state <= add;
               else
                  calc2_cnt    <= calc2_cnt + 1;
               end if;
            when add =>
               Hj(0) <= a + Hj(0);
               Hj(1) <= b + Hj(1);
               Hj(2) <= c + Hj(2);
               Hj(3) <= d + Hj(3);
               Hj(4) <= e + Hj(4);
               Hj(5) <= f + Hj(5);
               Hj(6) <= g + Hj(6);
               Hj(7) <= h + Hj(7);
               Mi_calc2_vld <= '1';
               M_calc2_eom  <= M_calc1_eom_hold;
               calc2_state <= idle;
            when others =>
               calc2_state <= idle;
         end case;

         -- stage3 outpute messege digest 
         if Mi_calc2_vld='1' and M_calc2_eom='1' then
            dgt <= Hj(0) & Hj(1) & Hj(2) & Hj(3) & Hj(4) & Hj(5) & Hj(6) & Hj(7);
            dgt_vld <= '1'; 
         else
            dgt <= (others=>'0');
            dgt_vld <= '0';
         end if;

      end if; -- arst_g/clk
   end process;

   M_len_mod512 <= conv_integer(unsigned(M_len(8 downto 0)));

   Mi_rd <= Mi_rd_vld and not add_blk;

end rtl;

