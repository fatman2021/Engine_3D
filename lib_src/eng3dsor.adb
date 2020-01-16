package body Engine_3D.Sorting is

  procedure Swap(a,b: in out integer) is c:integer; begin c:= a;a:= b;b:= c; end;
  pragma Inline(Swap);

Procedure Sort_faces(o: in out object_3D) is
  -- Faces with the HIGHEST Z-val is placed last in Order
  -- (was descending before 1.IV.1999)
  last_face : constant natural:= o.Num_of_faces;
  Center_Z  : Center_array renames o.Center_Z( 1 .. last_face );
  Order     : Index_array   renames o.Order;

  subtype Item is integer;  -- z-values (sorting key)
  subtype Index is integer; -- face numbers

-- Adaptated from Swing's Ada class library
-- URL: http://fedelma.astro.univie.ac.at/web/home.html
-- Copyright (c) 1995 Swing Informationssysteme GmbH        All rights reserved
-- tanzer@swing.co.at                   Glasauergasse 32, A--1130 Wien, Austria

  PROCEDURE Sort_Quick IS

    TYPE Side IS (Left,right);
    TYPE Sides IS ARRAY (Side) OF Index;
    Stack: ARRAY (1..32) OF Sides;           -- Up To 2^32 Elements
    S: INTEGER := 1;
    I, J, L, R: Index;
    H: Item;

  BEGIN
    Stack(S) := (1, last_face);

    WHILE S > 0 LOOP
        L := Stack(S)(Left);
        R := Stack(S)(Right);
        S := S - 1;
        WHILE L < R LOOP
            i := L;
            j := R;
            H := Center_Z((L+R)/2);
            LOOP
                LOOP
                    I := Index'SUCC(I);
                    EXIT WHEN Center_Z(I) >= H;
                END LOOP;
                LOOP
                    J := Index'PRED(J);
                    EXIT WHEN H >= Center_Z(J);
                END LOOP;

                EXIT WHEN J <= I;
                Swap(Center_Z(I), Center_Z(J));
                Swap(Order(I),    Order(J));
            END LOOP;

            IF J-L < R-I THEN
                IF i < R THEN                -- Stack(I, R)
                    S := S + 1;
                    Stack(S) := (I, R);
                END IF;
                R := J;                      -- Proceed In(L, J)
            ELSE
                IF L < j THEN                -- Stack(L, J)
                    S := S + 1;
                    Stack(S) := (L, J);
                END IF;
                L := I;                      -- Proceed In(I, R)
            END IF;
        END LOOP;
    END LOOP;
  END Sort_Quick;

  PROCEDURE Sort_Straight IS
    i: Index;
    h: Item; ho: Integer;

   BEGIN
    FOR j IN 2..last_face LOOP
        h  := Center_Z (j);
        ho := Order (j);
        i := j;
        WHILE (i > 1) AND THEN (h < Center_Z (Index'PRED (i))) LOOP
            Center_Z (i):= Center_Z (Index'PRED (i));
            Order (i)   := Order    (Index'PRED (i));
            i           := Index'PRED (i);
        END LOOP;
        Center_Z (i) := h;
        Order (i)   := ho;
    END LOOP;
   END Sort_Straight;

  PROCEDURE Sort_Quick_Straight IS

  BEGIN
      IF Index'POS(last_face) > 14 THEN
          Sort_Quick;
      ELSE
          Sort_Straight;
      END IF;
--      Sort_Straight;
  END Sort_Quick_Straight;

  PROCEDURE Sort_Shell IS
    T        : INTEGER := 3;
    I, K, IK : Index;
    Y        : Item;
    YO       : Integer;
    H   : CONSTANT ARRAY (1..12) OF INTEGER :=
          (1, 4, 13, 40, 121, 364, 1093,
          3280, 9841, 28444, 85333, 256000);

  BEGIN
    WHILE T < H'LAST AND THEN Index'VAL(H(T)) < last_face LOOP
        T := T + 1;
    END LOOP;
    T := T - 2;
    FOR S IN REVERSE 1..T LOOP
        k := Index'VAL(H (S));
        FOR j IN Index'SUCC(K)..last_face LOOP
            i := Index'VAL(Index'POS(J) - Index'POS(K));
            Y  := Center_Z(J);
            YO := Order(J);
            WHILE I >= 1 AND THEN Y < Center_Z(I) LOOP
                IK:= Index'VAL(Index'POS(I)+Index'POS(K));
                Center_Z(IK) := Center_Z(I);
                Order(IK)    := Order(I);
                i := Index'VAL(Index'POS(I) - Index'POS(K));
            END LOOP;
            IK:= Index'VAL(Index'POS(I)+Index'POS(K)); 
            Center_Z(IK) := Y;
            Order(IK)   := YO;
        END LOOP;
    END LOOP;
  END Sort_Shell;

  -- This procedure has been written in Ada by Jerome Delcourt
  -- from the idea of Karl-Dietrich Neubert 
  -- Adapated to context (key+order)

  PROCEDURE FlashSort1(V : IN OUT Center_array) IS
    M : CONSTANT Index := V'LENGTH / 10 + 1;
    K : Index := M;
    NMove : Index := 0;
    Hold, Flash, Swapp : Item;
    HoldO, FlashO, SwappO : Integer;
    J : Index := 1;
    IndexVMax : Index := V'FIRST;
    VMin : Item := Item'LAST;
    L : ARRAY(1..M) OF Index := (OTHERS => 0);
    C1 : FLOAT;
    THRESHOLD: constant:= 80;
  BEGIN
    ---------------------
    -- CLASS FORMATION --
    ---------------------
    FOR I IN V'RANGE LOOP
      IF (V(I) > V(IndexVMax)) THEN
        IndexVMax := I;
      ELSIF (V(I) < VMin) THEN
        VMin := V(I);
      END IF;
    END LOOP;
    -- If VMin = V(IndexVMax), V is already sorted...
    IF (VMin = V(IndexVMax)) THEN
      RETURN;
    END IF;
    C1 := FLOAT(M-1) / FLOAT(V(IndexVMax) - VMin);
    FOR I IN V'RANGE LOOP
      K := 1 + Index(C1 * FLOAT(V(I) - VMin));
      L(K) := L(K) + 1;
    END LOOP;
    FOR K IN 2..M LOOP
      L(K) := L(K) + L(K-1);
    END LOOP;
    --     Swap(V(1), V(IndexVMax));
    --     Swap(Order(1),    Order(IndexVMax));
    Swapp:=  V(1);  V(1):= V(IndexVMax); V(IndexVMax):= Swapp;
    SwappO:= Order(1); Order(1):= Order(IndexVMax);   Order(IndexVMax):= SwappO;
    -----------------
    -- PERMUTATION --
    -----------------
    WHILE (NMove < (V'LAST - 1)) LOOP
      WHILE (J > L(K)) LOOP
        J := J + 1;
        K := 1 + Index(C1 * FLOAT(V(J) - VMin));
      END LOOP;
      Flash  := V(J);
      FlashO := Order(J);
      WHILE (J /= L(K) + 1) LOOP
        K := 1 + Index(C1 * FLOAT(Flash - VMin));
        --         Swap(Flash,  V(L(K)));
        --         Swap(FlashO, Order(L(K)));
        Swapp:=  Flash;  Flash:= V(L(K)); V(L(K)):= Swapp;
        SwappO:= FlashO; FlashO:= Order(L(K));   Order(L(K)):= SwappO;
        L(K) := L(K) - 1;
        NMove := NMove + 1;
      END LOOP;
    END LOOP;
    -----------------------------------------------
    -- Choice of RECURSION or STRAIGHT INSERTION --
    -----------------------------------------------
    for k in L'first .. L'Last-1 loop
      declare nx: Index:= L(k+1) - L(k);
      begin
        if false and nx > THRESHOLD then -- use recursion
          Flashsort1( V( L(k)+1 .. L(k)+nx ) );
        else                                  -- use insertion sort
          FOR I IN REVERSE L(k)+1..L(k+1)-1 LOOP

--          FOR I IN REVERSE V'FIRST..V'Last-2 LOOP
            IF (V(I + 1) < V(I)) THEN
              Hold  := V(I);
              HoldO := Order(I);
              J := I;
              WHILE (V(J + 1) < Hold) LOOP
                V(J) := V(J+1);
                Order(J)    := Order(J+1);
                J := J+1;
              END LOOP;
              V(J) := Hold;
              Order(J)    := HoldO;
            END IF;
          END LOOP;

        end if;
      end;
    end loop;
  END FlashSort1;

  begin              -- We can test all sorts of sorting...

-- 5.IV.1999: o.Center_Z is filled using previous sorting -> no reset ->
-- should speedup a new sorting for an object not rotating too fast!

--    Sort_Straight;
--    Sort_Quick;
--    Sort_Quick_Straight;
--    Sort_Shell;
    FlashSort1( Center_Z );
  end Sort_faces;

end Engine_3D.Sorting;
