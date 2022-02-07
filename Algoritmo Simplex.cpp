#include "stdio.h"
#include "stdlib.h"
#include "locale.h"

/*
 -------------------------------------------------------------------------------------------------------
  Author: Wellison Rodrigues
 
  Trabalho Apresentado à Disciplina Programação Linear
 
  Prof. Luis Fernando Maia
 -------------------------------------------------------------------------------------------------------
*/


//-------------------------------------------- MATRIZ --------------------------------------------------

double** alocaMatriz(double** matriz, int n, int m){ 
	
	matriz = (double**) malloc (n * sizeof(double));
	
	if(matriz == NULL){ 
	    printf("\nMemmoria Insufiente!");
	    exit(1);
    }
	
	for(int i=0; i<n; i++){
	
		matriz[i] = (double*) malloc (m * sizeof(double));
	
		if(matriz[i] == NULL){ 
	       printf("\nMemmoria Insufiente!");
	       exit(1);
        }
	}
	
	return matriz;	
}

//------------------------------------------------------------------------------------------------------

void lerMatriz(double** mat, int lin, int col){
	
	printf("\nInsira o Tableau (copia e cola a Matriz):\n");
	
	for(int i=0; i<lin; i++){
		for(int j=0; j<col; j++){
			scanf("%lf", &mat[i][j]);
		}
	}
}

//------------------------------------------------------------------------------------------------------

void imprimirMatriz(double** mat, int lin, int col){
	
	printf("\n\n");
	for(int i=0; i<lin; i++){
		for(int j=0; j<col; j++){
			printf("%.3lf\t", mat[i][j]);
		}
		printf("\n");
	}
}

//------------------------------------------------------------------------------------------------------

void liberaMatriz(double** mat, int lin){
		for (int i=0; i < lin; i++){
	        free (mat[i]);
        }     
        free (mat);
        
		//printf("\nMemoria liberada!");
}

//---------------------------------------------- SIMPLEX -----------------------------------------------

bool basicaInicial(double** mat, int lin, int col){
    
	bool test = 0;
	int vet[lin], soma=0;
    
    for(int i=0; i<lin; i++){
  		vet[i] = 1;
	}
	
	for(int i=0; i<lin; i++){
	   for(int j=0; j<col; j++){
		    if(mat[i][j] == 1 || mat[i][j] == 0 && mat[lin-1][j] == 0){ //verificando base
		          if(mat[lin-1][col-1] == 0){ //verifica FO
		          	 vet[i] = 0; //recebe 0 quando indetificar uma variavel na base
				  }
		    }
	    }
    }
    
    for(int i=0; i<lin; i++){
  		soma += vet[i];
	}
	
	if(soma == 0){
		test = 1;
	}
	
	return test;
}

//------------------------------------------------------------------------------------------------------

int solucaoOtima(double** mat, int lin, int col){ 
	
	double Menor = mat[lin-1][0];
	int colunaEntra=0;
    
	for(int j=0; j<col-1; j++){
		if(mat[lin-1][j] < Menor){ //encontra o mais negativo da funcao objetivo
			Menor = mat[lin-1][j];
			colunaEntra = j;
		}
	}	
	
	if(Menor >= 0){
		return -1; //solucao otima
	}
	else{
		return colunaEntra; //posicao do menor	
	}
	
}

//------------------------------------------------------------------------------------------------------

int menorRazao(double** mat, int lin, int col, int colunaEntra){ 
	
	double razao;
	double razaoMenor = 1000000;
	int linhaSai;
	
	for(int j=0; j<col; j++){
		for(int i=0; i<lin-1; i++){
			
			if(colunaEntra == j && mat[i][j] != 0){	
				if(mat[i][col-1] > 0 && mat[i][j] > 0){
					
					razao = mat[i][col-1]/mat[i][j];
				
					if(razao < razaoMenor){
						razaoMenor = razao; //razao positiva
						linhaSai = i; //linha que sai recebe o indice
					}
				}
			}
		}
	}
	return linhaSai;
}

//------------------------------------------------------------------------------------------------------

void pivotearTableau(double** mat, int lin, int col, int linhaPivo, int baseEntra){
		float elemento_a_zerar;
		float pivo = mat[linhaPivo][baseEntra];
		int i, j=0;
		
		for(j=0; j<col; j++){
			mat[linhaPivo][j] = mat[linhaPivo][j]/pivo;
		}
		
		for(i=0; i<lin; i++){
			if(i != linhaPivo){
				elemento_a_zerar = -mat[i][baseEntra];
				
				for(j=0; j<col; j++){
					mat[i][j] = mat[i][j]+(mat[linhaPivo][j]*elemento_a_zerar);
				}
			}
		}
}

//------------------------------------------------------------------------------------------------------

void solucaoPPL(double** mat, int lin, int col){
	
	double vet[col-1];
	int i, j;
	
	for(j=0; j<col-1; j++){
		for(i=0; i<lin-1; i++){
			if(mat[i][j] == 1){
				vet[j] = mat[i][col-1];
			}
		}
	}
	
	printf("\nSolucao: (");
	for(j=0; j<col-1; j++){
		printf("%.2lf;", vet[j]);
	}
	printf("\b) = %.2lf", mat[lin-1][col-1]);
}

//------------------------------------------------------------------------------------------------------

void ilimitado(double** mat, int lin, int col, int colunaEntra){
	
	bool teste = 0;
	double elemento_a_zerar;
	int vet[lin];
	int soma = 0;
	
	for(int i=0; i<lin; i++){
		vet[i] = 1;
	}
	
	
	for(int i=0; i<lin; i++){
		elemento_a_zerar = mat[i][colunaEntra];
		
		if(elemento_a_zerar <= 0){
			vet[i] = 0; // vetor auxiliar recebe zero para cada valor negativo ou zerado da coluna que entra
		}	
	}
	for(int i=0; i<lin; i++){
		soma += vet[i]; // o somatorio do vetor auxiliar 
	}
	
	if(soma == 0){ // se o somatorio do vetor auxiliar igual a zero, significa que a coluna é zerada/negativa
		printf("\n\tPPL eh Ilimitado\n");
		printf("\nA prox coluna a entrar é zerada/negativa e nao há quem sair da base)\n");
		exit(1);
	}
}

//------------------------------------------------------------------------------------------------------

int multiplaSolucoes(double** mat, int lin, int col){
	
	int i, j;
	int indice, soma=0;
	int vet[lin-1] = {1,1};
	
	for(j=0; j<col-1; j++){
	   for(i=0; i<lin-1; i++){
            if(mat[i][j] != 1 && mat[i][j] != 0){ //fora da base
		     vet[i] = 0;
			 indice = j;       	  
            }			  
	    }
	}
	
    for(i=0; i<lin-1; i++){
        soma+=vet[i]; //soma igual a zero, a coluna possui elementos diferentes de 0 e 1
    }
	 
	if(soma == 0 && mat[lin-1][indice] == 0){ 
	    return indice;
    }else{
    	return -1; //nao é multipla
	}
 
}
 
//---------------------------------------------- TABLEAU -----------------------------------------------

void tableauDinamico(){
	
	double** tableau = NULL;
	int lin, col;
						
	printf("Qtd de linha e de colunas da matriz: ");
	scanf("%d %d", &lin, &col);
	
	tableau = alocaMatriz(tableau, lin, col);
	
	lerMatriz(tableau, lin, col);
	
	int indiceColunaEntra, indiceLinhaSai, colunaAtual;
	
	
    indiceColunaEntra = solucaoOtima(tableau, lin, col); // vendo se eh otima, caso nao seja, retorna -1 e encerra
    
	while(indiceColunaEntra != -1){ //ao sair achou uma solucao otima
		
		ilimitado(tableau, lin, col, indiceColunaEntra); //verificando se eh ilimitado
		indiceLinhaSai = menorRazao(tableau, lin, col, indiceColunaEntra);
	    pivotearTableau(tableau, lin, col, indiceLinhaSai, indiceColunaEntra);
		
		imprimirMatriz(tableau, lin, col);
	    indiceColunaEntra = solucaoOtima(tableau, lin, col);
	}
	
	if(indiceColunaEntra == -1){ //teste multiplas solucoes
		int indice;
	
		indice = multiplaSolucoes(tableau, lin, col);
		
		if(indice != -1){
  		    solucaoPPL(tableau, lin, col);
  		    
			printf("\n\tPPL com Multipla Solucao\n");
			printf("(Há coluna fora da base que está zerada na FO podendo entrar na base. Mas que nao altera a linha FO)\n");
		
			indiceLinhaSai = menorRazao(tableau, lin, col, indice);
	    	pivotearTableau(tableau, lin, col, indiceLinhaSai, indice);
		
			imprimirMatriz(tableau, lin, col);
			solucaoPPL(tableau, lin, col);
			exit(1);
		}
	
	}
	
	if(indiceColunaEntra == -1){
		printf("\n\tPPL com Solução Ótima Única\n");
		solucaoPPL(tableau, lin, col);
		exit(1);
	}
    
	liberaMatriz(tableau, lin);
	
}

//------------------------------------------------------------------------------------------------------

void tableauArquivo(char nome[]){
	
	FILE *arq;
	double** tableau;
    int lin, col;
	
	arq = fopen(nome, "r");
	
    if(arq == NULL){
    	printf("Erro de Leitura");
    	exit(1);
	} 
	
	fscanf(arq,"%d %d",&lin,&col);

    tableau = alocaMatriz(tableau, lin, col);
  
    for(int i = 0; i < lin; i++){
	    for(int j = 0; j < col; j++){
	      fscanf(arq,"%lf",&tableau[i][j]); //armazenar valores do arquivo na matriz
	    }
    }
   
	imprimirMatriz(tableau, lin, col);
	
	int indiceColunaEntra, indiceLinhaSai, colunaAtual;
	
	if(basicaInicial(tableau, lin, col) == 0){
        printf("O tableau nao tem basica inicial\n");
        exit(1);
	}
	
    indiceColunaEntra = solucaoOtima(tableau, lin, col); // vendo se eh otima, caso nao seja, retorna -1 e encerra
    
	while(indiceColunaEntra != -1){ //ao sair achou uma solucao otima
	
		ilimitado(tableau, lin, col, indiceColunaEntra); //verificando se eh ilimitado
		indiceLinhaSai = menorRazao(tableau, lin, col, indiceColunaEntra);
	    pivotearTableau(tableau, lin, col, indiceLinhaSai, indiceColunaEntra);
		
		imprimirMatriz(tableau, lin, col);
	    indiceColunaEntra = solucaoOtima(tableau, lin, col);
	}
	
	if(indiceColunaEntra == -1){ //teste multiplas solucoes
		int indice;
	
		indice = multiplaSolucoes(tableau, lin, col);
		
		if(indice != -1){
  		    solucaoPPL(tableau, lin, col);
  		    
			printf("\n\n\tMultipla Solução\n");
			printf("Há coluna fora da base que está zerada na FO podendo entrar na base.\nMas que nao altera a linha FO)\n");
		
			indiceLinhaSai = menorRazao(tableau, lin, col, indice);
	    	pivotearTableau(tableau, lin, col, indiceLinhaSai, indice);
		
			imprimirMatriz(tableau, lin, col);
			solucaoPPL(tableau, lin, col);
			exit(1);
		}
	
	}
	
	if(indiceColunaEntra == -1){
		printf("\n\tPPL com Solução Ótima Única\n");
		solucaoPPL(tableau, lin, col);
		exit(1);
	}
    
	liberaMatriz(tableau, lin);
  	fclose(arq);
}

//------------------------------------------------------------------------------------------------------

int main(){
	setlocale(LC_ALL, "Portuguese");
	
//	tableauDinamico(); //para inserçao de qualquer matriz


//	-------------------- tableau com arquivos --------------------------

	char nomeArq[] = "PPL_Solucao_Unica.txt";
	tableauArquivo(nomeArq);
	
//	char nomeArq2[] = "PPL_Ilimitado.txt";
//	tableauArquivo(nomeArq2);

//	char nomeArq3[] = "PPL_Multiplas_Solucoes.txt";
//	tableauArquivo(nomeArq3);
	
	
}
