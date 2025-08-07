       IDENTIFICATION DIVISION.
       PROGRAM-ID. LOGIN-MODULE.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT EMPLOYEE-FILE ASSIGN TO "employee.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS EMPLOYEE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD EMPLOYEE-FILE.
       01 EMPLOYEE-RECORD.
           05 FILE-EMPLOYEE-ID    PIC X(10).
           05 FILE-PASSWORD       PIC X(20).
           05 FILE-NAME           PIC X(30).
           05 FILE-ROLE           PIC X(10).
                

       WORKING-STORAGE SECTION.
         01 EMPLOYEE-ID        PIC X(10).
         01 EMPLOYEE-PASSWORD  PIC X(20).
         01 EMPLOYEE-NAME      PIC X(30).
         01 EMPLOYEE-ROLE      PIC X(10).
         01 FOUND-IT-FLAG      PIC X VALUE 'N'.
         01 EOF                PIC X VALUE 'N'.
         01 EMPLOYEE-STATUS    PIC XX.
        
      * user input for employee id
         01 EMPLOYEE           PIC X(10).
      * user input for password
         01 ENTERED-PASSWORD   PIC X(20).
      * password counter
         01 PASSWORD-ATTEMPTS  PIC 9 VALUE 0.
      *> variable to store the user role
         01 USER-ROLE          PIC X(10).  
      *> variable for cashier menu choice
         01 CASHIER-CHOICE     PIC 9.

      *> variable for SCAN-ITEM
         01 SCAN-EXIT          PIC X VALUE 'N'.
         01 ITEM-BARCODE       PIC X(20).

      *> variable for exiting PERFORM loops
         01 EXIT-LOOP-FLAG     PIC X VALUE 'N'.

       PROCEDURE DIVISION.
       EMPLOYEE-LOGIN. 
           OPEN INPUT EMPLOYEE-FILE
           IF EMPLOYEE-STATUS NOT = "00"
               DISPLAY "ERROR: Could not open employee.dat file." 
               DISPLAY "Please check if it exists."
               STOP RUN
           END-IF

           DISPLAY "employee.dat opened successfully for input."   

           DISPLAY "Please Enter Employee ID: "
           ACCEPT EMPLOYEE

           IF EMPLOYEE = SPACES
                DISPLAY "Error: Employee ID cannot be blank."
                GO TO EMPLOYEE-LOGIN
           END-IF.

           IF LENGTH OF EMPLOYEE > 10
                DISPLAY "ERROR: Employee ID exceeds maximum length " 
                DISPLAY "of 10 digits."
                GO TO EMPLOYEE-LOGIN
           END-IF.

           IF EMPLOYEE NOT NUMERIC
                DISPLAY "ERROR: Employee ID must be numeric."
                GO TO EMPLOYEE-LOGIN
           END-IF.

           IF LENGTH OF EMPLOYEE NOT = 10
               DISPLAY "ERROR: Employee ID must be 10 digits long."
               GO TO EMPLOYEE-LOGIN
           END-IF.

           PERFORM UNTIL EOF = 'Y'
             READ EMPLOYEE-FILE
                AT END
                  MOVE 'Y' TO EOF
                NOT AT END
                 IF FILE-EMPLOYEE-ID = EMPLOYEE
                  MOVE 'Y' TO FOUND-IT-FLAG
                  MOVE FILE-PASSWORD TO EMPLOYEE-PASSWORD
                  MOVE FILE-NAME TO EMPLOYEE-NAME
                  MOVE FILE-ROLE TO EMPLOYEE-ROLE
                  EXIT PERFORM
                 END-IF
             END-READ
           END-PERFORM.
           
           IF FOUND-IT-FLAG = 'Y'
               DISPLAY "Employee found: Hi, " EMPLOYEE-NAME
           ELSE
               DISPLAY "ERROR: Invalid Employee ID."
               GO TO EMPLOYEE-LOGIN
           END-IF.
           
           *> Prompt for password **********************
           PERFORM UNTIL PASSWORD-ATTEMPTS = 3 OR EXIT-LOOP-FLAG = 'Y'
            DISPLAY "Please enter your password: "
            ACCEPT ENTERED-PASSWORD
      
             IF FUNCTION TRIM(ENTERED-PASSWORD) = SPACE
                DISPLAY "ERROR: Password cannot be blank."
                ADD 1 TO PASSWORD-ATTEMPTS
              ELSE 
                  IF 
                       ENTERED-PASSWORD = FILE-PASSWORD
                       MOVE 'Y' TO EXIT-LOOP-FLAG
                    ELSE
                       DISPLAY "ERROR: Incorrect password."
                       ADD 1 TO PASSWORD-ATTEMPTS
                  END-IF
             END-IF
           END-PERFORM

           IF ENTERED-PASSWORD NOT = FILE-PASSWORD
              DISPLAY "ERROR: Too many failed attempts. Access denied."
           END-IF
           
           DISPLAY "Login successful. Welcome, " EMPLOYEE-NAME "!"
           MOVE EMPLOYEE-ROLE TO USER-ROLE
           DISPLAY "Your role is: " USER-ROLE

           
           IF USER-ROLE = "MANAGER"
               PERFORM MANAGER-MENU
           ELSE IF USER-ROLE = "CASHIER"
               PERFORM CASHIER-MENU
           ELSE
               DISPLAY "ERROR: Unknown role. Access denied."
               CLOSE EMPLOYEE-FILE
               STOP RUN
           END-IF.

           CLOSE EMPLOYEE-FILE.
           STOP RUN.



           *> PARAGRAPHS ******************************
           *> *****************************************
           
       MANAGER-MENU.
               DISPLAY "Welcome to the Manager Menu."
               *> Manager menu logic here
           .
       CASHIER-MENU.
            DISPLAY "Welcome to the Cashier Menu."
               *> Cashier menu logic here
           DISPLAY "======== CASHIER MENU ========"
           DISPLAY "1. Scan Item"
           DISPLAY "2. Apply Discount"
           DISPLAY "3. Cancel Item"
           DISPLAY "4. Change Quantity"
           DISPLAY "5. Change Price"
           DISPLAY "6. Void Item"
           DISPLAY "7. Suspend Sale"
           DISPLAY "8. Exit"
           DISPLAY "==============================="
           DISPLAY "Select an option (1 - 8):"
           ACCEPT CASHIER-CHOICE  

             IF CASHIER-CHOICE = SPACES
               DISPLAY "ERROR: Choice cannot be blank."
               GO TO CASHIER-MENU
             END-IF.

             IF CASHIER-CHOICE NOT NUMERIC OR CASHIER-CHOICE < 1 OR 
                CASHIER-CHOICE > 8
               DISPLAY "ERROR: Invalid choice."
               DISPLAY "Please enter a number between 1 and 8."
               GO TO CASHIER-MENU
             END-IF.
           
           EVALUATE CASHIER-CHOICE
             WHEN 1 PERFORM SCAN-ITEM
             WHEN 2 PERFORM APPLY-DISCOUNT
             WHEN 3 PERFORM CANCEL-ITEM
             WHEN 4 PERFORM CHANGE-QUANTITY
             WHEN 5 PERFORM CHANGE-PRICE
             WHEN 6 PERFORM VOID-ITEM
             WHEN 7 PERFORM SUSPEND-SALE
             WHEN 8 DISPLAY "Exiting cashier menu..."
             WHEN OTHER DISPLAY "Invalid option. Please try again."
           END-EVALUATE.

          *> Sub PARAGRAPHS for Cashier Menu Options
       SCAN-ITEM.
           DISPLAY "Entering Scan Mode. Type 'M' to return to menu"
           PERFORM UNTIL SCAN-EXIT = 'Y'
             DISPLAY "Scan item: "
             ACCEPT ITEM-BARCODE
             IF FUNCTION TRIM(ITEM-BARCODE) = "M"
                MOVE 'Y' TO SCAN-EXIT
                 DISPLAY "Returning to Cashier Menu..."
                ELSE IF FUNCTION TRIM(ITEM-BARCODE) = SPACE   
                        DISPLAY "ERROR: Item barcode "
                        DISPLAY "cannot be blank."
                        ELSE IF LENGTH OF ITEM-BARCODE NOT = 20
                                DISPLAY "ERROR: Item barcode "  
                                DISPLAY "must be exactly 20 digits."
                                ELSE IF ITEM-BARCODE NOT NUMERIC
                                        DISPLAY "ERROR: Item barcode " 
                                        DISPLAY "must be numeric."
                                        ELSE
                                            *> Process the scanned item
                                            PERFORM PROCESS-ITEM
                                            *> Logic for processing 
                                            *>the scanned item
                                            DISPLAY "Item scanned "
                                            DISPLAY "successfully: " 
                                            ITEM-BARCODE
                                            *> Reset the barcode for next scan
                                            MOVE SPACES TO ITEM-BARCODE
                                     END-IF 
                             END-IF
                     END-IF
             END-IF 
             
           END-PERFORM.
           


       PROCESS-ITEM.
              DISPLAY "Processing item: " ITEM-BARCODE
              *> Logic for processing the scanned item
           .



       APPLY-DISCOUNT.
               DISPLAY "Applying discount..."
                *> Logic for applying a discount
           .

       CANCEL-ITEM.
               DISPLAY "Cancelling item..."
                *> Logic for cancelling an item
           .

       CHANGE-QUANTITY.
               DISPLAY "Changing quantity..."
                *> Logic for changing quantity
           .

       CHANGE-PRICE.
               DISPLAY "Changing price..."
                *> Logic for changing price
           .

       VOID-ITEM.
               DISPLAY "Voiding item..."
                *> Logic for voiding an item
           .

       SUSPEND-SALE.
               DISPLAY "Suspending sale..."
                *> Logic for suspending a sale
           .
