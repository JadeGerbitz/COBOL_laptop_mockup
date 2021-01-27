      *Author: Carson Gerbitz
      /Class: COBOL 2230-01
      /Date: 2/24/2019
      /Description: This program takes in customer info and sales 
      /info for a mock up laptop sale. Then it calculates dicounts, 
      /sales tax, and totals.

       identification division.
       program-id. Program1.

       data division.
       working-storage section.
       01 CUSTOMER-NAME            PIC A(15).
       01 CUSTOMER-ADDRESS         PIC A(15).
       01 CUSTOMER-CITY-STATE-ZIP  PIC A(15)9(5).
       01 CUSTOMER-PHONE           PIC 9(3)A9(3)A9(4).
       01 DISPLAY-AMOUNT           PIC 9(4).9(2) VALUE 0000.00.
       01 COMPUTER.
           05 DESCRIPTION          PIC A(30).
           05 PRICE                PIC 9(4)V9(2) VALUE 0000.00.
           05 DISCOUNT             PIC 9(4)V9(2) VALUE 0000.00.
           05 DISCOUNT-PCT-MID     PIC 9V9(2) VALUE 0.05.
           05 DISCOUNT-PCT-HIGH    PIC 9V9(2) VALUE 0.010.
           05 SUB-TOTAL            PIC 9(4)V9(2) VALUE 0000.00.
           05 NET-SALE             PIC 9(4)V9(2) VALUE 0000.00.
           05 SALES-TAX            PIC 9(4)V9(2) VALUE 0000.00.
           05 SALES-TAX-PCT        PIC 9V9(3) VALUE 0.075.
           05 INVOICE-TOTAL        PIC 9(4)V9(2) VALUE 0000.00.
       01 ACCESSORIES.
           05 DISCOUNT-PCT-MID     PIC 9V9(3) VALUE 0.03.
           05 DISCOUNT-PCT-HIGH    PIC 9V9(3) VALUE 0.05.
           05 DISCOUNT             PIC 9(4)V9(2) VALUE 0000.00.
           05 TOTAL                PIC 9(4)V9(2) VALUE 0000.00.
           05 DESCRIPTION          PIC A(15).
           05 LOOP-COUNT           PIC 9 VALUE 1.
           05 PRICE                PIC 9(4)V9(2) VALUE 0000.00.

       procedure division.
      *This paragraph gathers the customer's personal information.    
           DISPLAY "Carson Gerbitz                 Computer Invoice".

           DISPLAY "Enter Customer name: "
               ACCEPT CUSTOMER-NAME.

           DISPLAY "Enter Customer Address: "
               ACCEPT CUSTOMER-ADDRESS.

           DISPLAY "Enter Customer City, State Zip: "
               ACCEPT CUSTOMER-CITY-STATE-ZIP.

           DISPLAY "Enter Customer's Phone #: "
               ACCEPT CUSTOMER-PHONE.

      *This paragraph gathers the description and price of the ordered
      /computer.    
           DISPLAY "ITEMS ORDERED | Price"

           DISPLAY "Enter Computer name: "
               ACCEPT DESCRIPTION OF COMPUTER.

           DISPLAY "Enter Computer price: "
               ACCEPT PRICE OF COMPUTER.

      *This paragraph handles the loop to gather prices of a customer's
      /accessories.
           DISPLAY "XXXXX to end accessories".

           PERFORM UNTIL LOOP-COUNT=6
               DISPLAY "Enter description of Accessory " LOOP-COUNT
               ACCEPT DESCRIPTION of ACCESSORIES
               IF DESCRIPTION of ACCESSORIES="XXXXX"
                   EXIT PERFORM
               END-IF
               DISPLAY "Enter price of Accessory " LOOP-COUNT
               ACCEPT PRICE OF ACCESSORIES
               ADD PRICE OF ACCESSORIES TO TOTAL OF ACCESSORIES
               COMPUTE LOOP-COUNT = LOOP-COUNT + 1
           END-PERFORM.

      *This paragraph handles the subtotal, discounts, tax, net sale, 
      /and invoice total calculations.
           ADD PRICE OF COMPUTER TO SUB-TOTAL OF COMPUTER.
           ADD TOTAL OF ACCESSORIES TO SUB-TOTAL OF COMPUTER.
           MOVE SUB-TOTAL OF COMPUTER TO DISPLAY-AMOUNT.
           DISPLAY "Subtotal: $" DISPLAY-AMOUNT.

      *This block calculates the discount for the computer.
           IF PRICE OF COMPUTER < 1000.00 THEN
               MOVE ZEROS TO DISCOUNT OF COMPUTER
           END-IF.
           if PRICE OF COMPUTER > 1000.00 AND PRICE OF COMPUTER < 
           1500.00 THEN
               MULTIPLY PRICE OF COMPUTER BY DISCOUNT-PCT-MID OF 
               COMPUTER GIVING DISCOUNT OF COMPUTER
           END-IF.
           IF PRICE OF COMPUTER > 1500.00 THEN
               MULTIPLY PRICE OF COMPUTER BY DISCOUNT-PCT-HIGH OF 
               COMPUTER GIVING DISCOUNT of COMPUTER
           END-IF.

      *This block calculates the discount for the accessories.
           IF TOTAL OF ACCESSORIES < 100.00 THEN
               MOVE ZEROS TO DISCOUNT OF ACCESSORIES
           END-IF.
           IF TOTAL OF ACCESSORIES > 100.00 AND TOTAL OF ACCESSORIES < 
           200.00 THEN
               MULTIPLY TOTAL OF ACCESSORIES BY DISCOUNT-PCT-MID of 
               ACCESSORIES GIVING DISCOUNT OF ACCESSORIES
           END-IF.
           IF TOTAL OF ACCESSORIES > 200.00 THEN
               MULTIPLY TOTAL OF ACCESSORIES BY DISCOUNT-PCT-HIGH of 
               ACCESSORIES GIVING DISCOUNT of ACCESSORIES
           END-IF.

      *This block calculates total discounts, net sale, and sales tax.
           ADD DISCOUNT OF ACCESSORIES TO DISCOUNT OF COMPUTER.
           SUBTRACT DISCOUNT OF COMPUTER FROM SUB-TOTAL OF COMPUTER 
               GIVING NET-SALE OF COMPUTER.
           MULTIPLY NET-SALE OF COMPUTER BY SALES-TAX-PCT OF COMPUTER 
               GIVING SALES-TAX OF COMPUTER.
           MOVE DISCOUNT OF COMPUTER TO DISPLAY-AMOUNT.
           DISPLAY "Discount: $" DISPLAY-AMOUNT "-".
           MOVE NET-SALE OF COMPUTER TO DISPLAY-AMOUNT.
           DISPLAY "Net sale: $" DISPLAY-AMOUNT.
           MOVE SALES-TAX TO DISPLAY-AMOUNT.
           DISPLAY "Sales tax: $" DISPLAY-AMOUNT.

      *This block calculates the invoice total and waits for the user
      /to close the program.
           ADD SALES-TAX OF COMPUTER TO INVOICE-TOTAL OF COMPUTER.
           ADD NET-SALE OF COMPUTER TO INVOICE-TOTAL OF COMPUTER.
           MOVE INVOICE-TOTAL TO DISPLAY-AMOUNT.
           DISPLAY "Invoice Total: $" DISPLAY-AMOUNT.
           DISPLAY "Press ENTER to end"
               ACCEPT INVOICE-TOTAL.

       end program Program1.