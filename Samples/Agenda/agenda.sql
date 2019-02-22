CREATE TABLE Contato(
  Codigo     INTEGER NOT NULL,
  Nome       VARCHAR(40) NOT NULL,
  Endereco   VARCHAR(40),
  Bairro     VARCHAR(20),
  Cidade     VARCHAR(20),
  CEP        VARCHAR(10),
  UF         CHAR(2),
  Fone       VARCHAR(20),
  Fax        VARCHAR(20),
  Celular    VARCHAR(20),
  Email      VARCHAR(40),
  CONSTRAINT PK_Telefone PRIMARY KEY(Codigo));


  
