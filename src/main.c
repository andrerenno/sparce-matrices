#include <stdlib.h>
#include <stdio.h>


typedef struct Elem {
    unsigned int col;
    float value;
    struct Elem* prox;
} Elem;

typedef struct Matrix {
    unsigned int num_of_rows;
    unsigned int num_of_cols;
    Elem** elements;
} Matrix;

Matrix* new_matrix(unsigned int, unsigned int);
void delete_matrix(Matrix*);
int matrix_add_elem(float, int, int, Matrix*);
float matrix_get_elem(int, int, Matrix*);
float sum_row(int, Matrix*);
float sum_col(int, Matrix*);
void add_two_rows(float, int, int, Matrix*);

Matrix* new_matrix(unsigned int m, unsigned int n){

    Matrix* matrix_ptr = malloc (sizeof(Matrix));

    matrix_ptr -> num_of_rows = m;
    matrix_ptr -> num_of_cols = n;

    matrix_ptr -> elements = malloc(m * sizeof(Elem));

    for (int i = 0; i < m; i++){
        matrix_ptr -> elements[i] = NULL;
    }

    return matrix_ptr;

}
Matrix* copy_matrix(Matrix* matrix_ptr){

    Matrix* matrix_cpy = (Matrix*) malloc(sizeof(Matrix));
    matrix_cpy -> elements = malloc((matrix_ptr -> num_of_rows) * sizeof(Elem));
    matrix_cpy -> num_of_rows = matrix_ptr -> num_of_rows;
    matrix_cpy -> num_of_cols = matrix_ptr -> num_of_cols;

    Elem* current_elem;
    for (int row = 0; row < matrix_ptr -> num_of_rows; row++){
        current_elem = matrix_ptr -> elements[row]; 
        while (current_elem != NULL){
            matrix_add_elem(current_elem -> value, row, current_elem -> col, matrix_cpy);
            current_elem = current_elem -> prox;
        }

    }
    return matrix_cpy;

}

void delete_matrix(Matrix* matrix_ptr){ 
    Elem* next_elem; 
    Elem* current_elem;
    for (int i = 0; i < (matrix_ptr -> num_of_rows); i++){
        current_elem = matrix_ptr -> elements[i];
        while (current_elem != NULL){
            next_elem = current_elem -> prox;
            free(current_elem);
            current_elem = next_elem;
        }
        matrix_ptr -> elements[i] = NULL;
    }
    free(matrix_ptr -> elements);
    free(matrix_ptr);
}

float matrix_get_det(Matrix* matrix_ptr){

    if (matrix_ptr -> num_of_rows != matrix_ptr -> num_of_cols)
        return  0;

    int order = matrix_ptr -> num_of_rows;
    int rowflag, colflag;
    float result = 1;
    for (int m = 0; m < order; m++){
        rowflag = 0;
        colflag = 0;
        for (int n = 0; n < order; n++){
            if (matrix_get_elem(m, n, matrix_ptr) != 0 ){
                rowflag = 1;
            }
            if (matrix_get_elem(n, m, matrix_ptr) != 0){
                colflag = 1;
            }
        }
        if (rowflag == 0 || colflag == 0)
            return 0;
    }

    /* Makes a copy first so I don't modify the original matrix */
    Matrix* tmp_matrix = copy_matrix(matrix_ptr);

    for(int col = 0; col < order; col++){ 
        int row; 

        /* If the main diagonal element for this column is 0, finds a row with a non-zero element and swaps with the main diagonal row */
        for (row = col; matrix_get_elem(row, col, tmp_matrix) == 0 && row < order; row++);
        if (row != col){
            Elem* tmp = tmp_matrix -> elements[row];
            tmp_matrix -> elements[row] = tmp_matrix -> elements[col];
            tmp_matrix -> elements[col] = tmp;
            result *= -1;
        }

        for (row = col + 1; row < order; row++){
            /* starts at element below main diagonal, continues until the end of the matrix */

            /* No need to change it if it's already 0 */
            if (matrix_get_elem(row, col, tmp_matrix) == 0){
                continue;
            }

            /*  Adds row "col" (with the current main diagonal element) multiplied by -1 * row_element/maind_diagonal_element to row below main diagonal    * 
             *  this should make every element below the main diagonal 0.                                                                                   *
             *  For example, take the matrix { {1, 2, 3} {4, 5, 6} {7, 8, 9} } and suppose the upper loop is at col = 0 and this loop is at row = 1         *
             *  this command is going to add row 0 {1 2 3} multiplied by -4/1 to row 1 {4, 5, 6} so now row 1 is {0, -3, -3}                                */

            add_two_rows((-1 * matrix_get_elem(row, col, tmp_matrix)) / matrix_get_elem(col, col, tmp_matrix), col, row, tmp_matrix);
        }
    }

    /* Once we get out of the loop, tmp_matrix is now triangular matrix, so we can get the determinant by multiplying the elements of the main diagonal */
    int diag;
    for (diag = 0; diag < order; diag++){
        printf("%.2f ", matrix_get_elem(diag, diag, tmp_matrix));
        result *= matrix_get_elem(diag, diag, tmp_matrix);
    }
    printf(")\n");

    return result;
}

int matrix_add_elem(float val, int row, int col, Matrix* matrix_ptr){

    if ((matrix_ptr -> num_of_rows) <= row || (matrix_ptr -> num_of_cols) <= col || (row < 0) || (col < 0)){
        printf("ERROR: Element is outside the range of the matrix!\n");
        return 0;
    }

    Elem* current_elem;
    Elem* prev;
    current_elem = matrix_ptr -> elements[row];
    prev = NULL;

    /* Find where value should go */
    while (current_elem != NULL && current_elem -> col < col){
        prev = current_elem;
        current_elem = current_elem -> prox;
    }

    if (prev == NULL){
        /* First element in row, need to modify Matrix struct */

        if (current_elem != NULL && current_elem -> col == col){
            /* There is something in this position already*/

            if (val == 0){
                matrix_ptr -> elements[row] = current_elem -> prox;
                free (current_elem);
            }
            else {
                current_elem -> value = val;
            }
        }

        else {
            /* There is nothing in this position */

            Elem* new_elem = malloc(sizeof(Elem));
            if (new_elem == NULL)
                return 0;

            new_elem -> col = col;
            new_elem -> value = val;
            new_elem -> prox = current_elem;

            matrix_ptr -> elements[row] = new_elem;
        }
    }
    else {

        if (current_elem != NULL && current_elem -> col == col){ 
            /* There is something in this position already*/

            if (val == 0){
                prev -> prox = current_elem -> prox;
                free (current_elem);
            }
            else {
                current_elem -> value = val;
            }
        }

        else { 
            /* There is nothing in this position */
            if (val != 0){

                Elem* new_elem = malloc(sizeof(Elem));

                new_elem -> col = col;
                new_elem -> value = val;
                new_elem -> prox = current_elem;

                prev -> prox = new_elem;
                return 1;
            }
        }

    }

    return 1;
}

float sum_row(int row, Matrix* matrix_ptr){
    if (row > matrix_ptr -> num_of_rows){
        return 0;
    }
    Elem* current_elem = matrix_ptr -> elements[row];
    float result = 0;
    while(current_elem != NULL){
        result += current_elem -> value;
        current_elem = current_elem -> prox;
    }
    return result;
}

float sum_col(int col, Matrix* matrix_ptr){
    if (col > matrix_ptr -> num_of_cols){
        return 0;
    }

    float result = 0;

    for (int i = 0; i < matrix_ptr -> num_of_rows; i++){
        result += matrix_get_elem(i, col, matrix_ptr);   
    }

    return result;
}


void add_two_rows (float multiplier, int row_from, int row_to, Matrix* matrix_ptr) {
    Elem* current_elem = matrix_ptr -> elements[row_from];
    printf("Added row %d multipled by %.2f to row %d\n", row_from, multiplier, row_to);
    while (current_elem != NULL){
        /* adds a NEW element with value (value(current_elem) + value(element in row_to with the same column)) to row_to */ 
        matrix_add_elem((multiplier * (current_elem -> value)) + matrix_get_elem(row_to, current_elem -> col, matrix_ptr), row_to, current_elem -> col, matrix_ptr);

        current_elem = current_elem -> prox;
    }
}

float matrix_get_elem(int row, int col, Matrix* matrix_ptr){ 

    if ((matrix_ptr -> num_of_rows) <= row || (matrix_ptr -> num_of_cols) <= col || (row < 0) || (col < 0)){
        return 0.0;
    }

    Elem* current_elem = matrix_ptr -> elements[row];
    /* Iterates through the linked list until it finds an element on the desired column (or until it passes the column) */
    while (current_elem != NULL && current_elem -> col < col){
        current_elem = current_elem -> prox;
    }

    if (current_elem == NULL || current_elem -> col != col){
        /* The element is not on the list */
        return 0.0;
    }

    return (current_elem -> value);

}

int main(void){

    int rows, cols;
    float val;
    rows = cols = val = 0;
    char input[10];
    char c = -1;

    Matrix* first_matrix = NULL;

    while(1){
        if (first_matrix == NULL){ 
            printf( "PRESS 1 TO CREATE A NEW MATRIX\n"
                    "PRESS 9 TO EXIT\n\n");
            
            if (c != -1)
                while ((c = getchar()) != '\n' && c != EOF); // Clear the input buffer

            printf(">>> ");

            fgets(input, 10, stdin);

            if (atoi(input) == 1){
                printf("Number of rows: ");
                scanf(" %d", &rows);
                printf("Number of rows: ");
                scanf(" %d", &cols);

                first_matrix = new_matrix(rows, cols);

                printf("Created matrix of size %dx%d \n\n", first_matrix -> num_of_rows, first_matrix -> num_of_cols);

                printf( "1: ADD ELEMENT\n"
                        "2: SEEK ELEMENT\n"
                        "3: SUM ROW\n"
                        "4: SUM COLUMN\n" );
                if (rows == cols)
                    printf("5: DETERMINANT\n");
                printf( "8: DELETE MATRIX\n"
                        "9: EXIT\n");
                while ((c = getchar()) != '\n' && c != EOF); // Clear the input buffer
            }

            else if (atoi(input) == 9)
                return 0;

        }
        else {

            printf(">>> ");


            fgets(input, 10, stdin);

            switch(atoi(input)){

                case 1:

                    printf("-- ADD A NUMBER --\n");

                    printf("Row: ");
                    scanf(" %d", &rows);
                    printf("Col: ");
                    scanf(" %d", &cols);

                    printf("Value: ");

                    scanf(" %f", &val);

                    int success = matrix_add_elem(val, rows, cols, first_matrix);

                    if (success == 1){
                        printf("Succesfully added element with value %.2f at row %d and column %d.\n", val, rows, cols);
                    }
                    else { 
                        printf("Failed to add element at row %d col %d.\n", rows, cols);
                    }
                    while ((c = getchar()) != '\n' && c != EOF); // Clear the input buffer

                    break;

                case 2:
                    printf("-- FIND A NUMBER --\n");

                    printf("Row: ");
                    scanf(" %d", &rows);
                    printf("Col: ");
                    scanf(" %d", &cols);

                    val = matrix_get_elem(rows, cols, first_matrix); 

                    printf("Value: %.2f\n", val);

                    while ((c = getchar()) != '\n' && c != EOF); // Clear the input buffer
                    break;

                case 3:
                    printf("-- SUM ROW --\n");

                    printf("Row: ");
                    scanf(" %d", &rows);

                    printf("Result: %.2f\n", sum_row(rows, first_matrix));
                    while ((c = getchar()) != '\n' && c != EOF); // Clear the input buffer

                    break;

                case 4:
                    printf("-- SUM COLUMN --\n");

                    printf("Column: ");
                    scanf(" %d", &cols);

                    printf("Result: %.2f\n", sum_col(cols, first_matrix));
                    while ((c = getchar()) != '\n' && c != EOF); // Clear the input buffer

                    break;

                case 5:
                    if (first_matrix -> num_of_rows != first_matrix -> num_of_cols)
                        break;
                    float det = matrix_get_det(first_matrix);
                    printf("Determinant: %.2f\n", det); 
                    break;
                case 8:

                    printf("Are you sure you want to delete the matrix? (y/N) ");

                    scanf(" %c", &c);
                    if (c == 'y' || c == 'Y'){
                        delete_matrix(first_matrix);
                        first_matrix = NULL;
                    }
                    break;

                case 9:

                    printf("Are you sure you want to exit? Your matrix will not be saved. (y/N) ");
                    scanf(" %c", &c);

                    if (c == 'y' || c == 'Y'){

                        delete_matrix(first_matrix);

                        return 0;
                    }
                    while ((c = getchar()) != '\n' && c != EOF); // Clear the input buffer
                    break;

                default: 
                    while ((c = getchar()) != '\n' && c != EOF); // Clear the input buffer
                    break;


            }
        }


    }

    return 0;

}
