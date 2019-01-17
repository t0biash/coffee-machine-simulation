with Ada.Text_IO, Ada.Unchecked_Deallocation, Ada.Exceptions, NT_Console;
use Ada.Text_IO, Ada.Exceptions, NT_Console;

procedure Main is
    type Coffee_Types is (Black, Americano, Cappuccino);
    type Screen_States is (Communicate_With_User, Communicate_With_Machine_Parts);

    Default_Background_Color : Color_Type := Get_Background;
    Default_Foreground_Color : Color_Type := Get_Foreground;
    Big_Black_Coffee : constant Positive := 40;
    Small_Black_Coffee : constant Positive := 25;
    Big_Americano_Coffee : constant Positive := 20;
    Small_Americano_Coffee : constant Positive := 10;
    Big_Cappuccino_Coffee : constant Positive := 20;
    Big_Cappuccino_Milk : constant Positive := 20;
    Small_Cappuccino_Coffee : constant Positive :=  10;
    Small_Cappuccino_Milk : constant Positive := 10;
    Big_Coffee_Water_Amount : constant Natural := 500;
    Small_Coffee_Water_Amount : constant Natural := 300;

    task TaskExceptionsLogger is
        entry Write_Message(Message : in String);
    end TaskExceptionsLogger;

    task body TaskExceptionsLogger is
        File : File_Type;
        Name : String := "logs.txt";
        AnyExceptions : Boolean := false;

        begin
            loop
                select
                    accept Write_Message(Message : in String) do
                        if AnyExceptions = false then
                            Create(File, Out_File, Name);
                        end if;
                        AnyExceptions := true;
                        Put_Line(File, Message);
                    end Write_Message;
                or
                    terminate;
                end select;
            end loop;
    end TaskExceptionsLogger;

    task CoffeeMachine is 
        entry Run;
    end CoffeeMachine;

    task type Screen is
        entry Choose_Coffee_Type(Coffee_Type : out Coffee_Types);
        entry Choose_Coffee_Size(Big_Coffee : out Boolean);
        entry Choose_Sugar_Amount(Sugar_Amount : out Integer);
        entry Receive_Messages_From_Machine_Parts(Message : in String; IncludeNewLine : in Boolean);
        entry Show_Coffee_Progress(Big_Coffee : in Boolean);
        entry Next_Coffee(Next_Coffee : out Boolean);
    end Screen;
    type InputScreen_Pointer_Type is access Screen;

    task type Controller is
        entry Run(Coffee_Type : in Coffee_Types; Big_Coffee : in Boolean; Sugar_Amount : in Integer; Screen : in InputScreen_Pointer_Type; Not_Enough_Products : out Boolean);
    end Controller;
    type Controller_Pointer_Type is access Controller;
    
    procedure Free_Controller is new Ada.Unchecked_Deallocation
        (Object => Controller, Name => Controller_Pointer_Type);
    procedure Free_InputScreen is new Ada.Unchecked_Deallocation
        (Object => Screen, Name => InputScreen_Pointer_Type);

    task body CoffeeMachine is
        Coffee_Type : Coffee_Types;
        Big_Coffee : Boolean;
        Sugar_Amount : Integer;
        Next_Coffee : Boolean;
        Not_Enough_Products : Boolean;
        Screen_Pointer : InputScreen_Pointer_Type;
        Controller_Pointer : Controller_Pointer_Type;

        begin
            accept Run;
            loop
                Screen_Pointer := new Screen;
                Controller_Pointer := new Controller;
                Screen_Pointer.Choose_Coffee_Type(Coffee_Type);
                Screen_Pointer.Choose_Coffee_Size(Big_Coffee);
                Screen_Pointer.Choose_Sugar_Amount(Sugar_Amount);
                Controller_Pointer.Run(Coffee_Type, Big_Coffee, Sugar_Amount, Screen_Pointer, Not_Enough_Products);
                if Not_Enough_Products then
                    Screen_Pointer.Receive_Messages_From_Machine_Parts("Brak wystarczajacej ilosci produktow. Przepraszamy !", true);
                end if;
                Screen_Pointer.Next_Coffee(Next_Coffee);
                if not Next_Coffee then
                    Free_Controller(Controller_Pointer);
                    Free_InputScreen(Screen_Pointer);
                    exit;
                end if;
            end loop;
        exception
            when E:others => 
                --Put_Line("Error: Task CoffeeMachine");
                --Put_Line(Exception_Name(E) & ": " & Exception_Message(E));
                TaskExceptionsLogger.Write_Message("Error: Task CoffeeMachine");
                TaskExceptionsLogger.Write_Message(Exception_Name(E) & ": " & Exception_Message(E));
    end CoffeeMachine;

    task body Screen is
        Screen_State : Screen_States := Communicate_With_User;
        Option : Integer := 0;
        Not_Enough_Products : Boolean := false;

        begin
            Clear_Screen;
            Set_Foreground (Cyan);

            loop
                select 
                    when Screen_State = Communicate_With_User =>
                        accept Choose_Coffee_Type(Coffee_Type : out Coffee_Types) do
                            Goto_XY(53, 0);
                            Put_Line("AUTOMAT DO KAWY");
                            while Option /= 1 and Option /= 2 and Option /= 3 loop
                                New_Line;
                                Put_Line("Wybierz rodzaj kawy:");
                                Put_Line("1 - kawa czarna");
                                Put_Line("2 - kawa americano");
                                Put_Line("3 - cappuccino");
                                begin
                                    Option := Integer'Value(Get_Line);
                                    exception 
                                        when others => 
                                            Set_Foreground (Red);
                                            Put_Line("Dozwolone sa jedynie wartosci liczbowe");
                                            Set_Foreground (Cyan);
                                end;
                            end loop;
                            if Option = 1 then
                                Coffee_Type := Black;
                            elsif Option = 2 then
                                Coffee_Type := Americano;
                            else
                                Coffee_Type := Cappuccino;
                            end if;
                            Option := 0;
                        end Choose_Coffee_Type;

                        accept Choose_Coffee_Size(Big_Coffee : out Boolean) do
                            while Option /= 1 and Option /= 2 loop
                                New_Line;
                                Put_Line("Wybierz wielkosc kawy:");
                                Put_Line("1 - kawa mala");
                                Put_Line("2 - kawa duza");
                                begin
                                    Option := Integer'Value(Get_Line);
                                    exception
                                        when others => 
                                            Set_Foreground (Red);
                                            Put_Line("Dozwolone sa jedynie wartosci liczbowe");
                                            Set_Foreground (Cyan);
                                end;
                            end loop;
                            if Option = 1 then
                                Big_Coffee := false;
                                Put_Line("Ok. W takim razie kawa mala.");
                            else
                                Big_Coffee := true;
                                Put_Line("Ok. W takim razie kawa duza.");
                            end if;
                            Option := 0;
                        end Choose_Coffee_Size;
                        
                        accept Choose_Sugar_Amount(Sugar_Amount : out Integer) do
                            loop
                                New_Line;
                                Put_Line("Podaj (w gramach) ilosc cukru:");
                                begin
                                    Sugar_Amount := Integer'Value(Get_Line);
                                    exception
                                        when others => 
                                            Set_Foreground (Red);
                                            Put_Line("Dozwolone sa jedynie wartosci liczbowe");
                                            Sugar_Amount := -1;
                                            Set_Foreground (Cyan);
                                end;
                                if Sugar_Amount >= 0 then
                                    exit;
                                else
                                    Set_Foreground (Magenta);
                                    Put_Line("Ilosc gram cukru powinna byc wieksza lub rowna 0");
                                    Set_Foreground (Cyan);
                                end if;
                            end loop;
                            Screen_State := Communicate_With_Machine_Parts;
                        end Choose_Sugar_Amount;
                or
                    when Screen_State = Communicate_With_Machine_Parts =>
                            accept Receive_Messages_From_Machine_Parts(Message : in String; IncludeNewLine : in Boolean) do
                                if IncludeNewLine then
                                    Put_Line(Message);
                                else
                                    Put(Message);
                                end if;
                            end Receive_Messages_From_Machine_Parts;
                or
                    when Screen_State = Communicate_With_Machine_Parts =>
                        accept Show_Coffee_Progress(Big_Coffee : in Boolean) do
                            Set_Foreground(Brown);
                            if Big_Coffee then
                                Clear_Screen;
                                Goto_XY(0, 2);
                                Put_Line("|    |");
                                Put_Line("|    |");
                                Put_Line("|    |");
                                Put_Line("|    |");
                                Put_Line("|    |");
                                Put_Line("|____|");
                                delay 1.5;
                                Clear_Screen;
                                Goto_XY(0, 2);
                                Put_Line("|    |");
                                Put_Line("|    |");
                                Put_Line("|    |");
                                Put_Line("|    |");
                                Put_Line("|~~~~|");
                                Put_Line("|____|");
                                delay 1.5;
                                Clear_Screen;
                                Goto_XY(0, 2);
                                Put_Line("|    |");
                                Put_Line("|    |");
                                Put_Line("|    |");
                                Put_Line("|~~~~|");
                                Put_Line("|    |");
                                Put_Line("|____|");
                                delay 1.5;
                                Clear_Screen;
                                Goto_XY(0, 2);
                                Put_Line("|    |");
                                Put_Line("|    |");
                                Put_Line("|~~~~|");
                                Put_Line("|    |");
                                Put_Line("|    |");
                                Put_Line("|____|");
                                delay 1.5;
                                Clear_Screen;
                                Goto_XY(0, 2);
                                Put_Line("|    |");
                                Put_Line("|~~~~|");
                                Put_Line("|    |");
                                Put_Line("|    |");
                                Put_Line("|    |");
                                Put_Line("|____|");
                                delay 1.5;
                                Clear_Screen;
                                Goto_XY(0, 2);
                                Put_Line("    * ");
                                Put_Line("   *  ");
                                Put_Line("  **  ");
                                Put_Line("|~~~~|");
                                Put_Line("|    |");
                                Put_Line("|    |");
                                Put_Line("|    |");
                                Put_Line("|    |");
                                Put_Line("|____|");
                                delay 1.5;
                             else 
                                Clear_Screen;
                                Goto_XY(0, 2);
                                Put_Line("|    |");
                                Put_Line("|    |");
                                Put_Line("|    |");
                                Put_Line("|____|");
                                delay 1.5;
                                Clear_Screen;
                                Goto_XY(0, 2);
                                Put_Line("|    |");
                                Put_Line("|    |");
                                Put_Line("|~~~~|");
                                Put_Line("|____|");
                                delay 1.5;
                                Clear_Screen;
                                Goto_XY(0, 2);
                                Put_Line("|    |");
                                Put_Line("|~~~~|");
                                Put_Line("|    |");
                                Put_Line("|____|");
                                delay 1.5;
                                Clear_Screen;
                                Goto_XY(0, 2);
                                Put_Line("    * ");
                                Put_Line("   *  ");
                                Put_Line("  **  ");
                                Put_Line("|~~~~|");
                                Put_Line("|    |");
                                Put_Line("|    |");
                                Put_Line("|____|");
                                delay 1.5;
                            end if;
                            Set_Foreground(Cyan);
                            New_Line;
                        end Show_Coffee_Progress;
                or
                    accept Next_Coffee(Next_Coffee : out Boolean) do
                        while Option /= 1 and Option /= 2 loop
                            New_Line;
                            Put_Line("Czy chcesz przygotowac nastepna kawe:");
                            Put_Line("1 - tak");
                            Put_Line("2 - nie");
                            begin
                                Option := Integer'Value(Get_Line);
                                exception
                                    when others => Put_Line("Dozwolone sa jedynie wartosci liczbowe");
                            end;
                        end loop;
                        if Option = 1 then
                            Screen_State := Communicate_With_User;
                            Next_Coffee := true;
                        else
                            Next_Coffee := false;
                            Put("Trwa wylaczanie");
                            for I in 1..3 loop
                                Put(".");
                                delay 1.0;
                            end loop;
                            Set_Background(Default_Background_Color);
                            Set_Foreground(Default_Foreground_Color);
                        end if;
                    end Next_Coffee;
                or
                    terminate;
                end select;
            end loop;
        exception
            when E:others => 
                --Put_Line("Error: Task Screen");
                --Put_Line(Exception_Name(E) & ": " & Exception_Message(E));
                TaskExceptionsLogger.Write_Message("Error: Task Screen");
                TaskExceptionsLogger.Write_Message(Exception_Name(E) & ": " & Exception_Message(E));
    end Screen;

    protected WaterContainer is
        procedure Get(W : in out Natural; Water_Amount : in Natural);
        function Check_Current_Amount return Natural;
        
        private 
            Water : Natural := 10000;
    end WaterContainer;

    protected CoffeeContainer is
        procedure Get(Chamber : in out Natural; Coffee_Amount : in Natural);
        function Check_Current_Amount return Natural;

        private 
            Coffee : Natural := 2000;
    end CoffeeContainer;

    protected MilkContainer is 
        procedure Get(Chamber : in out Natural; Milk_Amount : in Natural);
        function Check_Current_Amount return Natural;

        private
            Milk : Natural := 2000;
    end MilkContainer;

    protected SugarContainer is
        procedure Get(Chamber : in out Natural; Sugar_Amount : in Natural);
        function Check_Current_Amount return Natural;
        
        private 
            Sugar : Natural := 2000;
    end SugarContainer;

    task type Heater(Big_Coffee : Boolean; Screen : InputScreen_Pointer_Type) is 
        entry Get_Water(Water_Amount : out Natural);
    end Heater;
    type Heater_Pointer_Type is access Heater;

    task type MixingChamber(Coffee_Type : Coffee_Types; Big_Coffee : Boolean; Sugar_Amount : Integer; Heater_Pointer : Heater_Pointer_Type; Screen : InputScreen_Pointer_Type) is
        entry Mix_Ingredients;
        entry Empty;
    end MixingChamber;
    type MixingChamber_Pointer_Type is access MixingChamber;

    procedure Free_MixingChamber is new Ada.Unchecked_Deallocation
        (Object => MixingChamber, Name => MixingChamber_Pointer_Type);
    procedure Free_Heater is new Ada.Unchecked_Deallocation
        (Object => Heater, Name => Heater_Pointer_Type);
        
    task body Controller is
        MixingChamber_Pointer : MixingChamber_Pointer_Type;
        Heater_Pointer : Heater_Pointer_Type;

        begin
            loop
                select 
                    accept Run(Coffee_Type : in Coffee_Types; Big_Coffee : in Boolean; Sugar_Amount : in Integer; Screen : in InputScreen_Pointer_Type; Not_Enough_Products : out Boolean) do
                        Not_Enough_Products := false;
                        if SugarContainer.Check_Current_Amount < Sugar_Amount then
                            Not_Enough_Products := true;
                        end if;
                        if Big_Coffee then
                            if WaterContainer.Check_Current_Amount < Big_Coffee_Water_Amount then
                                Not_Enough_Products := true;
                            end if;
                            if Coffee_Type = Black then 
                                if CoffeeContainer.Check_Current_Amount < Big_Black_Coffee then
                                    Not_Enough_Products := true;
                                end if;
                            elsif Coffee_Type = Americano then
                                if CoffeeContainer.Check_Current_Amount < Big_Americano_Coffee then
                                    Not_Enough_Products := true;
                                end if;
                            else
                                if CoffeeContainer.Check_Current_Amount < Big_Cappuccino_Coffee then
                                    Not_Enough_Products := true;
                                end if;
                                if MilkContainer.Check_Current_Amount < Big_Cappuccino_Milk then
                                    Not_Enough_Products := true;
                                end if;
                            end if;
                        else
                            if WaterContainer.Check_Current_Amount < Small_Coffee_Water_Amount then
                                Not_Enough_Products := true;
                            end if;
                            if Coffee_Type = Black then 
                                if CoffeeContainer.Check_Current_Amount < Small_Black_Coffee then
                                    Not_Enough_Products := true;
                                end if;
                            elsif Coffee_Type = Americano then
                                if CoffeeContainer.Check_Current_Amount < Small_Americano_Coffee then
                                    Not_Enough_Products := true;
                                end if;
                            else
                                if CoffeeContainer.Check_Current_Amount < Small_Cappuccino_Coffee then
                                    Not_Enough_Products := true;
                                end if;
                                if MilkContainer.Check_Current_Amount < Small_Cappuccino_Milk then
                                    Not_Enough_Products := true;
                                end if;
                            end if;
                        end if;
                        if not Not_Enough_Products then
                            if Big_Coffee then
                                Heater_Pointer := new Heater(Big_Coffee, Screen);
                            else
                                Heater_Pointer := new Heater(Big_Coffee, Screen);
                            end if;
                            MixingChamber_Pointer := new MixingChamber(Coffee_Type, Big_Coffee, Sugar_Amount, Heater_Pointer, Screen);

                            MixingChamber_Pointer.Mix_Ingredients;
                            MixingChamber_Pointer.Empty;
                            Free_Heater(Heater_Pointer);
                            Free_MixingChamber(MixingChamber_Pointer);
                        end if;
                    end Run;
                or
                    terminate;
                end select;
            end loop;
        exception
            when E:others => 
                --Put_Line("Error: Controller Task");
                --Put_Line(Exception_Name(E) & ": " & Exception_Message(E));
                TaskExceptionsLogger.Write_Message("Error: Controller Task");
                TaskExceptionsLogger.Write_Message(Exception_Name(E) & ": " & Exception_Message(E));
    end Controller;
 
    protected body WaterContainer is
        procedure Get(W : in out Natural; Water_Amount : in Natural) is
            begin
                Water := Water - Water_Amount;
                W := Water_Amount;
        end Get;

        function Check_Current_Amount return Natural is (Water);
    end WaterContainer;

    protected body MilkContainer is 
        procedure Get(Chamber : in out Natural; Milk_Amount : in Natural) is
            begin
                if Milk >= Milk_Amount then
                    Milk := Milk - Milk_Amount;
                    Chamber := Chamber + Milk_Amount;
                end if;
        end Get;

        function Check_Current_Amount return Natural is (Milk);
    end MilkContainer;

    protected body CoffeeContainer is 
        procedure Get(Chamber : in out Natural; Coffee_Amount : in Natural) is 
            begin
                if Coffee >= Coffee_Amount then
                    Coffee := Coffee - Coffee_Amount;
                    Chamber := Chamber + Coffee_Amount;
                end if;
        end Get;

        function Check_Current_Amount return Natural is (Coffee);
    end CoffeeContainer;

    protected body SugarContainer is
        procedure Get(Chamber : in out Natural; Sugar_Amount : in Natural) is
            begin
                if Sugar >= Sugar_Amount then
                    Sugar := Sugar - Sugar_Amount;
                    Chamber := Chamber + Sugar_Amount;
                end if;
        end Get;

        function Check_Current_Amount return Natural is (Sugar);
    end SugarContainer;

    task body Heater is
        Water : Natural := 0;
        
        begin
            if Big_Coffee then
                WaterContainer.Get(Water, Big_Coffee_Water_Amount);
            else
                WaterContainer.Get(Water, Small_Coffee_Water_Amount);
            end if;
            Screen.Receive_Messages_From_Machine_Parts("Trwa podgrzewanie wody", false);
            for I in Integer range 1..5 loop
                Screen.Receive_Messages_From_Machine_Parts(".", false);
                delay 1.0;
            end loop;
            New_Line;
            accept Get_Water(Water_Amount : out Natural) do
                Water_Amount := Water;
                Water := 0;
            end Get_Water;
        exception
            when E:others => 
                --Put_Line("Error: Heater Task");
                --Put_Line(Exception_Name(E) & ": " & Exception_Message(E));
                TaskExceptionsLogger.Write_Message("Error: Heater Task");
                TaskExceptionsLogger.Write_Message(Exception_Name(E) & ": " & Exception_Message(E));
    end Heater;

    task body MixingChamber is
        Chamber : Natural := 0;

        begin
            if Big_Coffee then
                if Coffee_Type = Black then 
                    CoffeeContainer.Get(Chamber, Big_Black_Coffee);
                    SugarContainer.Get(Chamber, Sugar_Amount);
                elsif Coffee_Type = Americano then
                    CoffeeContainer.Get(Chamber, Big_Americano_Coffee);
                    SugarContainer.Get(Chamber, Sugar_Amount);
                else
                    MilkContainer.Get(Chamber, Big_Cappuccino_Milk);
                    CoffeeContainer.Get(Chamber, Big_Cappuccino_Coffee);
                    SugarContainer.Get(Chamber, Sugar_Amount);
                end if;
            else
                if Coffee_Type = Black then 
                    CoffeeContainer.Get(Chamber, Small_Black_Coffee);
                    SugarContainer.Get(Chamber, Sugar_Amount);
                elsif Coffee_Type = Americano then
                    CoffeeContainer.Get(Chamber, Small_Americano_Coffee);
                    SugarContainer.Get(Chamber, Sugar_Amount);
                else
                    MilkContainer.Get(Chamber, Small_Cappuccino_Milk);
                    CoffeeContainer.Get(Chamber, Small_Cappuccino_Coffee);
                    SugarContainer.Get(Chamber, Sugar_Amount);
                end if;
            end if;
            accept Mix_Ingredients do
                Heater_Pointer.Get_Water(Chamber);
            end Mix_Ingredients;
            accept Empty do
                Screen.Receive_Messages_From_Machine_Parts("Napelniam kubek kawa...", false);
                delay 1.5;
                Screen.Show_Coffee_Progress(Big_Coffee);
                Screen.Receive_Messages_From_Machine_Parts("Kawa gotowa do odbioru", true);
            end Empty;
        exception
            when E:others => 
                --Put_Line("Error: MixingChamber Task");
                --Put_Line(Exception_Name(E) & ": " & Exception_Message(E));
                TaskExceptionsLogger.Write_Message("Error: MixingChamber Task");
                TaskExceptionsLogger.Write_Message(Exception_Name(E) & ": " & Exception_Message(E));
    end MixingChamber;

    begin
        CoffeeMachine.Run;
end Main;