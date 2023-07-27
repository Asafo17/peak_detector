library ieee; 
use ieee.std_logic_1164.all;
use ieee.std_logic_signed.all;
use ieee.numeric_std.all;
use work.common_pack.all;

entity dataConsume is
  	port (
	    clk: in std_logic;
		reset: in std_logic; -- synchronous reset
		start: in std_logic; -- goes high to signal data transfer
		numWords_bcd: in BCD_ARRAY_TYPE(2 downto 0);
		ctrlIn: in std_logic;
		ctrlOut: out std_logic;
		data: in std_logic_vector(7 downto 0);
		dataReady: out std_logic;
		byte: out std_logic_vector(7 downto 0);
		seqDone: out std_logic;
		maxIndex: out BCD_ARRAY_TYPE(2 downto 0);
		dataResults: out CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1)  -- index 3 holds the peak
  	);
end dataConsume;
 
architecture behavioural of dataConsume is

    -- Define states used in machine, seven working states and one IDLE state   
    type state_type is (IDLE,S1,S2,S3,S4,S5,S6,S7);
    
    -- Define signals used in code and set default values
    signal currentstate, nextstate: state_type;
    signal numWords_counter, numWords_memory, bcd_total, one_int, ten_int, hundred_int: integer range -1 to 999 := 0;
    signal sequence_complete: std_logic := '0';
    signal one_bin, ten_bin, hundred_bin: std_logic_vector(3 DOWNTO 0);
    signal one_bcd, ten_bcd, hundred_bcd: integer range 0 to 999;
    signal A: integer := 10; -- Constants used in BCD/INT conversion
    signal B: integer := 100;
    signal maxByte : std_logic_vector(7 downto 0) := "00000000";
    
    -- Define registers used in code and set default values. Registers are used to store input/output values and avoid latches.
    -- Every register is controlled by an enable signal and the master reset
    -- Ensures entire circuit is synchronous and synced to the clock
	signal numWords_reg: BCD_ARRAY_TYPE(2 DOWNTO 0) := ("0000", "0000", "0000");
    signal maxIndex_reg: BCD_ARRAY_TYPE(2 DOWNTO 0) := ("0000", "0000", "0000");
    signal ctrlIn_delay_reg: std_logic := '0';
    signal ctrlIn_reg: std_logic := '0';
    signal ctrlOut_reg: std_logic := '0';
    signal data_reg: std_logic_vector (7 downto 0) := "00000000";
    signal seqDone_reg: std_logic := '0';
    signal dataReady_reg: std_logic := '0';
	signal dataResults_reg: CHAR_ARRAY_TYPE(0 to RESULT_BYTE_NUM-1); -- Index 3 holds the peak
	signal byte_reg1: std_logic_vector (7 downto 0) := "00000000";
    signal byte_reg2: std_logic_vector (7 downto 0) := "00000000";
	signal byte_reg3: std_logic_vector (7 downto 0) := "00000000";
	signal byte_memory1: std_logic_vector (7 downto 0) := "00000000";
	signal byte_memory2: std_logic_vector (7 downto 0) := "00000000";
	signal byte_memory3: std_logic_vector (7 downto 0) := "00000000";
	signal byte_memory4: std_logic_vector (7 downto 0) := "00000000";
	signal byte_next1: std_logic_vector (7 downto 0) := "00000000";
	signal byte_next2: std_logic_vector (7 downto 0) := "00000000";
	signal byte_next3: std_logic_vector (7 downto 0) := "00000000";
    
    -- Define enable signals that control the registers
    signal en_ctrlIn_delay_reg: std_logic := '0';
    signal en_ctrlIn_reg: std_logic := '0';
    signal en_ctrlOut_reg: std_logic := '0';
    signal en_data_reg: std_logic := '0';
    signal en_numWords_reg: std_logic := '0';
    signal en_numWords_counter: std_logic := '0';
    signal en_numWords_memory: std_logic := '0';
    signal en_seqDone_reg: std_logic := '0';
    signal en_dataReady_reg: std_logic := '0';
    signal en_bcd_to_int: std_logic := '1'; -- Default ON values as BCD/INT conversion is always required
    signal en_int_to_bcd: std_logic := '1';
    signal en_dataResults_reg: std_logic := '0';
    signal en_byte_reg: std_logic := '0';
    signal en_memory: std_logic := '0';
    signal en_next: std_logic := '0';
    signal en_maxIndex_reg: std_logic := '0';
    
    signal reset_seqDone_reg: std_logic := '0';
    signal reset_dataReady_reg: std_logic := '0';
    
----------------------------------------------------------------------------------------------------------------------------------------------

BEGIN

    -- Connect registers with respective outputs
	ctrlOut <= ctrlOut_reg;
	byte <= data_reg;
	dataResults <= dataResults_reg;
	maxIndex <= maxIndex_reg;
--    seqDone <= seqDone_reg;
--    dataReady <= dataReady_reg;
    
    combi_nextState: PROCESS(currentstate,clk,reset)
    BEGIN
        
        -- These values are reset to default value of 0 unless assigned otherwise in a state to avoid generating latches
        en_data_reg <= '0';
        en_ctrlOut_reg <= '0';
        en_numWords_counter <= '0';
        en_numWords_memory <= '0';
        en_next <= '0';
        en_byte_reg <= '0';
        en_maxIndex_reg <= '0';
        dataReady <= '0';
        seqDone <= '0';
           
        CASE currentState IS
        
        WHEN IDLE => 
        -- IDLE state waits for 'start' input from command processor 
            en_ctrlIn_delay_reg <= '0';
            IF start = '1' THEN
                en_ctrlIn_reg <= '1'; 
                en_numWords_reg <= '1'; -- numWords only available after start command is issued
                nextState <= S1;
             ELSE
                en_numWords_reg <= '0';
                nextstate <= IDLE; 
             END IF;
        
        WHEN S1 =>
        -- Enable registers used in two-phase protocol
            en_ctrlOut_reg <= '1'; 
            en_ctrlIn_delay_reg <= '1'; 
            nextState <= S2;
            
        WHEN S2 =>
            -- Check if ctrlIn value has changed, signalling that data is ready 
            en_memory <= '0';
            IF ctrlIn_reg = '1' AND ctrlIn_delay_reg = '0' THEN
                nextState <= S3;
            ELSIF ctrlIn_reg = '0' AND ctrlIn_delay_reg = '1' THEN
                nextState <= S3;
            ELSE
                nextState <= S2;
            END IF;
        
        WHEN S3 =>
        -- Issue dataReady signal and check if byte sequence is complete, if so then issue seqDone
            dataReady <= '1';
            en_data_reg <= '1';
            en_numWords_counter <= '1';
            en_byte_reg <= '1';
            IF sequence_complete = '1' THEN
                seqDone <= '1';
                nextState <= IDLE;             
            ELSE
                seqDone <= '0';
                nextState <= S4;
            END IF; 
            
        WHEN S4 =>
        -- Check for peak byte
            IF data_reg > maxByte THEN
                nextstate <= S5;
            ELSE    
                nextstate <= S1;
            END IF;
        
        WHEN S5 =>
        -- If peak byte detected, set current byte as new peak byte
        -- Enable memory for previous three bytes
            maxByte <= data_reg;
            en_memory <= '1';
            en_numWords_memory <= '1';
            nextstate <= S6;

        WHEN S6 =>
        -- Enable memory for future three bytes
        -- Check if next three bytes have been processed and check there is no new peak 
            en_next <= '1';
            IF (numWords_counter - numWords_memory) < 3 AND data_reg < maxByte THEN
                nextstate <= S5;
            ELSE
                nextstate <= S7;
            END IF;
        
        WHEN S7 =>
        -- Enable registers to output dataResults and maxIndex to command processor 
            en_maxIndex_reg <= '1';
            en_dataResults_reg <= '1';
            nextstate <= S1;
            
        WHEN OTHERS =>
            nextstate <= IDLE;
            
        END CASE;
    END PROCESS;  
    
----------------------------------------------------------------------------------------------------------------------------------------------

    int_to_bcd_maxIndex : PROCESS (clk, reset)
    -- Calculates the index of the peak byte
    BEGIN
        IF rising_edge(clk) THEN  
            IF reset = '1' THEN                                           
                maxIndex_reg(0) <= "0000";
                maxIndex_reg(1) <= "0000";
                maxIndex_reg(2) <= "0000";
            ELSIF en_int_to_bcd = '1' THEN
                -- Convert integer to BCD and store in maxIndex_reg register
                hundred_bcd <= numWords_memory/B; -- Compute the hundreds digit
                ten_bcd <= (numWords_memory - hundred_bcd*B)/A; -- Compute the tens digit
                one_bcd <= numWords_memory - (hundred_bcd*B) - (ten_bcd*A); -- Compute the ones digit
                -- Convert the BCD digits to binary and store them in maxIndex_reg register
                maxIndex_reg(2) <= std_logic_vector(to_unsigned(hundred_bcd, 4));
                maxIndex_reg(1) <= std_logic_vector(to_unsigned(ten_bcd, 4));
                maxIndex_reg(0) <= std_logic_vector(to_unsigned(one_bcd, 4));
            END IF;
        END IF;
    END PROCESS;
    
    bcd_to_int : PROCESS (clk, reset)
    BEGIN
    -- Converts bcd value from numWords_reg into an integar value
        IF rising_edge(clk) THEN  
            IF reset = '1' THEN                                           
                bcd_total <= 0;
            ELSIF en_bcd_to_int <= '1' THEN
                -- Extract each binary digit from numWords_reg and convert it into integer
                one_bin <= numWords_reg(0);
                ten_bin <= numWords_reg(1);
                hundred_bin <= numWords_reg(2);                
                one_int <= to_integer(unsigned(one_bin));
                ten_int <= to_integer(unsigned(ten_bin))*A;
                hundred_int <= to_integer(unsigned(hundred_bin))*B;
                -- Calculate the integer value from the extracted binary digits
                bcd_total <= one_int + ten_int + hundred_int;
            END IF;
        END IF;
    END PROCESS;
    
    counter_numWords : PROCESS (clk, reset)
    BEGIN
    -- Counter which tracks number of bytes that have been processed, checks if requested number of bytes has been processed
        IF rising_edge(clk) THEN
            IF reset = '1' THEN
                numWords_counter  <= -1;
            ELSIF en_numWords_counter = '1' THEN
                numWords_counter <= numWords_counter + 1;
                -- Check if the requested number of bytes has been processed
                IF numWords_counter > (bcd_total - 2)  THEN
                    numWords_counter <= -1;
                    sequence_complete <= '1';
                ELSE
                    sequence_complete <= '0';
                END IF;
            -- Update the numWords_memory signal with the current value of the counter when en_numWords_memory signal is high
            ELSIF en_numWords_memory = '1' THEN
                numWords_memory <= numWords_counter;
            END IF;
        END IF;
    END PROCESS;

    reg_byte : PROCESS (clk, reset)
    -- Stores received bytes in registers
    BEGIN
        IF rising_edge(clk) THEN
            IF reset = '1' THEN
                byte_reg1 <= "00000000";
                byte_reg2 <= "00000000";
                byte_reg3 <= "00000000";
            ELSIF en_byte_reg = '1' THEN 
                -- Shift bytes in registers and store new byte
                byte_reg1 <= data_reg;
                byte_reg2 <= byte_reg1;
                byte_reg3 <= byte_reg2;
            END IF;
        END IF;
    END PROCESS;
	
    memory_previous : PROCESS (clk, reset)
    -- Stores the peak byte and the three bytes preceding it 
    BEGIN
       IF rising_edge(clk) THEN
           IF reset = '1' THEN
                byte_memory1 <= "00000000";
                byte_memory2 <= "00000000";
                byte_memory3 <= "00000000";
           ELSIF en_memory = '1' THEN
               -- Shift bytes in memory and store new byte
               byte_memory1 <= data_reg;
               byte_memory2 <= byte_reg1;
               byte_memory3 <= byte_reg2;
               byte_memory4 <= byte_reg3;
           END IF;
       END IF;
    END PROCESS;

    memory_next : PROCESS (clk, reset)
    -- Stores three bytes following the peak byte in registers
    BEGIN
       IF rising_edge(clk) THEN
           IF reset = '1' THEN
               byte_next1 <= "00000000";
               byte_next2 <= "00000000";
               byte_next3 <= "00000000";
           ELSIF en_next <= '1' THEN
               -- Store new byte in the appropriate next byte register
               IF (numWords_counter - numWords_memory) = 1 THEN
                   byte_next1 <= data_reg;
               ELSIF (numWords_counter - numWords_memory) = 2 THEN
                   byte_next2 <= data_reg;
               ELSIF (numWords_counter - numWords_memory) = 3 THEN
                   byte_next3 <= data_reg;	       
               END IF;
           END IF;
       END IF;
    END PROCESS;
    
	reg_data : PROCESS (clk, reset)
	--Store each byte received in a register, value updated when en_data_reg enabled
	BEGIN
		IF rising_edge(clk) THEN
			IF reset = '1' THEN
				data_reg <= "00000000";
			ELSIF en_data_reg = '1' THEN 
				data_reg <= data;
			END IF;
		END IF;
	END PROCESS;
	
    reg_numWords : PROCESS (clk, reset)
    -- Stores numWords signal in register, updated when en_numWords_reg is enabled
    BEGIN
        IF rising_edge(clk) THEN
            IF reset = '1' THEN
                numWords_reg <= ("0000", "0000", "0000");
            ELSIF en_numWords_reg = '1' THEN
                -- Update register with new value
                numWords_reg <= numWords_bcd;
            END IF;
        END IF;
    END PROCESS;

    reg_dataResults : PROCESS (clk, reset)
    -- Process to store data in a register called dataResults_reg
    BEGIN
      IF rising_edge(clk) then
        IF reset = '1' then
          dataResults_reg <= (others => (others => '0'));
        ELSIF en_dataResults_reg = '1' then
          -- Update the register with new values
          -- Store the four bytes of data from byte_memory
          dataResults_reg(0) <= byte_memory4;
          dataResults_reg(1) <= byte_memory3;
          dataResults_reg(2) <= byte_memory2;
          dataResults_reg(3) <= byte_memory1;
          -- Store the three bytes of data from byte_next
          dataResults_reg(4) <= byte_next1;
          dataResults_reg(5) <= byte_next2;
          dataResults_reg(6) <= byte_next3;
        END IF;
      END IF;
    END PROCESS;
    
    reg_dataReady : PROCESS (clk, reset) 
    -- Process to store seqDone in register before outputting to command processor 
    BEGIN 
        IF rising_edge(clk) THEN
            IF reset_dataReady_reg = '1' THEN
                dataReady_reg <= '0';
            ELSIF en_dataReady_reg = '1' THEN
                dataReady_reg <= '1';
            END IF;
        END IF;        
    END PROCESS;
    
    reg_seqDone : PROCESS (clk, reset) 
    -- Process to store seqDone in register before outputting to command processor 
    BEGIN 
        IF rising_edge(clk) THEN
            IF reset_seqDone_reg = '1' THEN
                seqDone_reg <= '0';
            ELSIF en_seqDone_reg = '1' THEN
                seqDone_reg <= '1';
            END IF;
        END IF;        
    END PROCESS;
            
	reg_CtrlIn : PROCESS (clk, reset)
    -- Stores ctrlIn signal in register, value updated when en_ctrlIn_reg is enabled
	BEGIN
		IF rising_edge(clk) THEN
			IF reset = '1' THEN
			    ctrlIn_reg <= '0';
		    ELSIF en_ctrlIn_reg = '1' THEN
		        ctrlIn_reg <= ctrlIn;
			END IF;
		END IF;
	END PROCESS;
	
	reg_CtrlIn_delay : PROCESS (clk, reset)
    -- Creates delayed ctrlIn signal, value updated when en_ctrlIn_delay_reg is enabled
	BEGIN
	   IF rising_edge(clk) THEN
	       IF reset = '1' THEN
	           ctrlIn_delay_reg <= '0';
	       ELSIF en_ctrlIn_delay_reg = '1' THEN
               ctrlIn_delay_reg <= ctrlIn_reg;
           END IF;
       END IF;
    END PROCESS;
	
	reg_CtrlOut : PROCESS (clk, reset)
    --Requests data from dataGen by inverting the ctrlOut signal, value updated when en_ctrlOut_reg is enabled
	BEGIN
		IF rising_edge(clk) THEN
			IF reset = '1' THEN
				ctrlOut_reg <= '0';
			ELSIF en_ctrlOut_reg = '1' THEN 
				ctrlOut_reg <= NOT ctrlOut_reg;
			END IF;
		END IF;
	END PROCESS;
	
    next_state: PROCESS(reset,clk)
    --Next state logic, processes next state on every rising clock edge
    BEGIN
        IF reset = '1'  THEN
            -- Reset to initial state
            currentState <= IDLE;
        ELSIF rising_edge(clk) THEN
            -- Update the current state with the next state
            currentState <= nextState;
        END IF;
    END PROCESS;

END BEHAVIOURAL;