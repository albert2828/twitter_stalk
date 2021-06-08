library(ggplot2)
library(dplyr)
library(lubridate)
library(wordcloud)
library(RColorBrewer)
library(tm)
library(tokenizers)

if(!dir.exists("graficas")){
            dir.create("graficas")
}

tuits_df <- read.csv("tuits_bhherto95.csv")
tuits_df <- tuits_df[,-1]
tuits_df$created <- as_datetime(tuits_df$created)
tuits_df <- tuits_df %>% filter(favoriteCount<20)

png(filename = "./graficas/distribucion_favs.png", width = 560, height = 320, units = "px", 
    pointsize = 12, bg = "white", res = NA, restoreConsole = TRUE)
tuits_df %>% ggplot(aes(x=favoriteCount))+
            geom_histogram(binwidth = 1, color="black")+
            theme_light()+
            labs(title="Distribución de favs en tuits",
                 x="Número de Favs",
                 y="")
dev.off()

png(filename = "./graficas/favs_por_dia.png", width = 560,
    height = 320, units = "px", pointsize = 12, bg="white", res = NA)
tuits_df %>% mutate(fecha=as.Date(created)) %>% 
            group_by(fecha) %>% summarise(favs = sum(favoriteCount)) %>%
            ggplot(aes(x=fecha, y=favs))+
            geom_line()+
            theme_light()+
            labs(title = "Evolución del número de favs por día")+
            scale_x_date(date_labels = "%Y %b %d", date_breaks = "1 month")+
            theme(axis.text.x = element_text(angle = 45, hjust = 1))
dev.off()

png(filename = "./graficas/tuits_por_dia_2021.png", width = 560,
    height = 320, units = "px", pointsize = 12, bg="white", res = NA)
p1 <- tuits_df %>% filter(created>=as.Date("2021-01-01")) %>%
            mutate(fecha=as.Date(created)) %>% 
            group_by(fecha) %>% summarise(tuits = n()) %>%
            ggplot(aes(x=fecha, y=tuits))+
            geom_line()+
            theme_light()+
            labs(title="Tuits por día en 2021",
                 x="",
                 y="")+
            scale_x_date(date_labels = "%Y %b %d", date_breaks = "1 month")+
            ylim(0,30)
p1
dev.off()

png(filename = "./graficas/favs_por_dia_2021.png", width = 560,
    height = 320, units = "px", pointsize = 12, bg="white", res = NA)
p2 <- tuits_df %>% filter(created>=as.Date("2021-01-01")) %>%
            mutate(fecha=as.Date(created)) %>% 
            group_by(fecha) %>% summarise(favs = sum(favoriteCount)) %>%
            ggplot(aes(x=fecha, y=favs))+
            geom_line()+
            theme_light()+
            labs(title="Favs por día en 2021",
                 x="",
                 y="")+
            scale_x_date(date_labels = "%Y %b %d", date_breaks = "1 month")+
            ylim(0,30)
p2 
dev.off()

png(filename = "./graficas/tuis_favs_por_dia_2021.png", width = 560,
    height = 320, units = "px", pointsize = 12, bg="white", res = NA)
grid.arrange(p1,p2,nrow=2)
dev.off()

png(filename = "./graficas/tuis_favs_por_diasemana.png", width = 560,
    height = 320, units = "px", pointsize = 12, bg="white", res = NA)
tuits_df %>% mutate(dia = wday(created, label = TRUE)) %>%
            group_by(dia) %>% 
            summarise(favs = mean(favoriteCount)) %>%
            ggplot(aes(x=dia, y=favs)) +
            geom_bar(stat = "identity")+
            theme_light()+
            labs(title = "Promedio de favs por día de la semana ")
dev.off()

png(filename = "./graficas/tuis_favs_por_hora_diasemana.png", width = 560,
    height = 320, units = "px", pointsize = 12, bg="white", res = NA)
tuits_df %>% mutate(hora = format(as.POSIXct(created,format="%H:%M:%S"),"%H"),
                          dia_sem = wday(created, label = T)) %>%
            group_by(hora, dia_sem) %>%
            summarise(favs=mean(favoriteCount))%>%
            ggplot(aes(x=hora, y=favs, color=dia_sem))+
            geom_point()+
            theme_light()+
            labs(title = "Promedio de favs por hora")
dev.off()

tuits_corpus <- Corpus(VectorSource(tuits_df$text), 
                       readerControl = list(readPlain, language="sp", load=TRUE))

tuits_corpus <- tm_map(tuits_corpus, content_transformer(tolower))
tuits_corpus <- tm_map(tuits_corpus, removePunctuation)
tuits_corpus <- tm_map(tuits_corpus, removeNumbers)
tuits_corpus <- tm_map(tuits_corpus, removeWords, stopwords("spanish"))
inspect(tuits_corpus[1:5])

subs <- function(s){
            s <- sub("¿", "", s)
            s <- sub("https*", "", s)
            return(s)
}

tuits_corpus <- tm_map(tuits_corpus, content_transformer(subs))
tuits_corpus <- tm_map(tuits_corpus, stripWhitespace)
inspect(tuits_corpus[1:5])
tuits_dtm <- tokenize_ngrams(sapply(tuits_corpus, as.character), n=1)
tuits_dtm <- unlist(tuits_dtm)
tuits_dtm <- data.frame(table(tuits_dtm))
tuits_dtm <- arrange(tuits_dtm, desc(Freq))

my_stopwords <- c("si", "d", "diagramadeven", "esteusermurio", "comacurez", "elcancerend",
                  "jajajaja", "jejeje", "xd", "jajaja", "jeje", "c", "natszendro",
                  "ay", "fotoenpared", "elizarial", "iamthecat", "patriciagaso", 
                  "i", "s", "is","of", "jaja","claudifonos", "eldelonceonce", 
                  "uf", "ufa", "pedo")
tuits_corpus <- tm_map(tuits_corpus, removeWords, my_stopwords)
tuits_corpus <- tm_map(tuits_corpus, stripWhitespace)
tuits_dtm <- tokenize_ngrams(sapply(tuits_corpus, as.character), n=1)
tuits_dtm <- unlist(tuits_dtm)
tuits_dtm <- data.frame(table(tuits_dtm))
tuits_dtm <- arrange(tuits_dtm, desc(Freq))
colnames(tuits_dtm) <- c("word", "freq")
tuits_dtm$word <- as.character(tuits_dtm$word)
png(filename = "./graficas/wordcloyd_tuits.png", width = 560,
    height = 320, units = "px", pointsize = 12, bg="white", res = NA)
layout(matrix(c(1, 2), nrow=2), heights=c(1, 4))
par(mar=rep(0, 4))
plot.new()
text(x=0.5, y=0.5, "Palabras más frequentes")
wordcloud(words = tuits_dtm$word, freq = tuits_dtm$freq, 
          main ="Title", min.freq = 10,max.words = 200, 
          random.order = F, rot.per = 0.35,
          colors = brewer.pal(8, "Set2"),
          scale = c(2.5,0.7))
dev.off()