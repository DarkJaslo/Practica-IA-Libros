#include <iostream>
#include <string>
#include <vector>
#include <map>
#include <set>
using namespace std;

map<string,string> titulosClips;
map<string,string> titulosNormal;
map<string,string> publicadorasClips;
map<string,string> publicadorasNormal;
map<string,string> autopublicadoresClips;
map<string,string> autopublicadoresNormal;
map<string,string> autoresClips;
map<string,string> autoresNormal;
vector<string> generos = {"Accion", "Aventura", "Ciencia ficcion", "Comedia", "Deportes", "Drama", "Fantasia", "Horror", "Misterio", "Psicologico", "Romance", "Slice of life", "Sobrenatural", "Suspense"};
vector<string> temas = {"Animales", "Artes", "Artes marciales", "Battle royale", "Carreras", "Delincuencia", "Detectives", "Escolar", "Espacio exterior", "Familiar", "Genero", "Gore", "Guerra", "Harem", "Historico", "Humor absurdo", "Idols", "Isekai", "Juegos de estrategia", "Magia", "Mahou shoujo", "Mitologia", "Musica", "Parodia", "Personajes adultos", "Postapocaliptico", "Reconfortante", "Robots", "Superpoderes", "Supervivencia", "Tradicional japones", "Tragedia", "Triangulo amoroso", "Vampiros", "Venganza", "Viajes en el tiempo", "Videojuegos", "Yaoi", "Yuri"};

struct MangaYAutoria
{
    string nombre;
    bool escribe;
    bool ilustra;
};

map<string,vector<MangaYAutoria>> mangasPorAutor;

template <typename T>
int binarySearch(int l, int r, const T& thing, const vector<T>& vec)
{
    if(l > r) return -1;
    int m = (l+r)/2;
    if(vec[m] > thing)
        return binarySearch(l,m-1,thing,vec);
    else if(vec[m] < thing)
        return binarySearch(m+1,r,thing,vec);
    return m;
}

template <typename T>
bool find(const T& thing, const vector<T>& vec)
{
    return binarySearch(0,vec.size()-1,thing,vec) >= 0;
}

void inserta(map<string,string>& m1, map<string,string>& m2, const string& key, const string& value)
{
    if(m1.find(key) != m1.end())
    {
        if(m1[key] != value) //problema
        {
            cerr << "Error: el nombre " << key << " ya se esta traduciendo como " << m1[key] << ", pero se quiere traducir por " << value << "\n";
            __throw_runtime_error("excepcion");
        }
    }
    else
    {
        m1[key] = value;
        m2[value] = key;
    }
}

string normToClips(const string& nombre)
{
    string res;
    res.reserve(nombre.length()+1);

    for(char c : nombre)
    {
        if(c >= 'A' and c <= 'Z') //mayus
        {
            c += ('a'-'A');
        }
        else if(c == 32) //space
        {
            c = '-';
        }
        else if(c == ',' or c == '.' or c == ':' or c == '!' or c == '?')
            continue;

        res += c;
    }

    return res;
}

string tipo(const string& t)
{
    if(t == "o")
    {
        return "One-shot";
    }
    else if(t == "s")
    {
        return "Serializado";
    }
    cerr << t << "\n";
    __throw_domain_error("mal input en tipo()");
}

string dif(const string& d)
{
    if(d == "b"){
        return "baja";
    }
    else if(d == "m"){
        return "media";
    }
    else if(d == "a"){
        return "alta";
    }
    cerr << d << "\n";
    __throw_domain_error("mal input en dif()");
}

string estado(const string& e)
{
    if(e == "ac"){
        return "acabado";
    }
    else if(e == "pu"){
        return "en publicacion";
    }
    else if(e == "pa"){
        return "en pausa";
    }
    else if(e == "ca"){
        return "cancelado";
    }
    cerr << e << "\n";
    __throw_domain_error("mal input en estado()");
}

string frec(const string& f)
{
    //1s|2s|1m|2m|3m|6m|ir {"semanal" "quincenal" "mensual" "bimestral" "trimestral" "semestral" "irregular"}
    if(f == "1s"){
        return "semanal";
    }
    else if(f == "2s"){
        return "quincenal";
    }
    else if(f == "1m"){
        return "mensual";
    }
    else if(f == "2m"){
        return "bimestral";
    }
    else if(f == "3m"){
        return "trimestral";
    }
    else if(f == "6m"){
        return "semestral";
    }
    else if(f == "ir"){
        return "irregular";
    }
    cerr << f << "\n";
    __throw_domain_error("mal input en frec()");
}

string metodo(const string& m)
{
    if(m == "d"){
        return "digital";
    }
    else if(m == "f"){
        return "fisico";
    }
    else if(m == "a"){
        return "ambos";
    }
    cerr << m << "\n";
    __throw_domain_error("mal input en metodo()");
}

bool autopublicado(const string& a)
    {
        if(a == "e")
        {
            return false;
        }
        else if(a == "a"){
            return true;
        }
        cerr << a << "\n";
        __throw_domain_error("mal input en autopublicado()");
        return false;
    }

struct Manga
{
    string titulo; //Normal
    string tipo;
    int tomos;
    int capitulos;
    string autor; //Clips
    string ilustrador; //Clips
    string publicadora; //Clips
    int copias;
    string dificultad;
    string estado;
    string frecuencia;
    string ini;
    string metodo;
    bool anime;
    float valoracion;
    int edad;
    vector<string> generos;
    vector<string> temas;
    bool autopublicado;

    string generosToClips()
    {
        string res;
        for(const string& g : generos)
        {
            res.append("  [");
            res.append(normToClips(g));
            res.append("]");
        }
        return res;
    }

    string temasToClips()
    {
        string res;
        for(const string& t : temas)
        {
            res.append("  [");
            res.append(normToClips(t));
            res.append("]");
        }
        return res;
    }

    string tieneAnime(){
        if(anime) return "TRUE";
        return "FALSE";
    }

    void toClips()
    {
        cout << "(";
        cout << "[" << titulosClips[titulo] << "] of " << tipo << "\n";
        cout << "\t(titulo  \"" << titulo << "\")\n";
        cout << "\t(publicado-por  [" << publicadora << "])\n";
        cout << "\t(escrito-por  [" << autor << "])\n";
        cout << "\t(ilustrado-por  [" << ilustrador << "])\n";
        if(generos.size() > 0)
            cout << "\t(pertenece-a" << generosToClips() << ")\n";
        if(temas.size() > 0)
            cout << "\t(trata-de" << temasToClips() << ")\n";
        if(tipo != "One-shot")
            cout << "\t(tomos  " << tomos << ")\n";
        cout << "\t(capitulos  " << capitulos << ")\n";
        cout << "\t(copias-vendidas  " << copias << ")\n";
        cout << "\t(dificultad-lectura  \"" << dificultad << "\")\n";
        if(tipo != "One-shot")
        {
            cout << "\t(estado-publicacion  \"" << estado << "\")\n";
            cout << "\t(frecuencia-publicacion  \"" << frecuencia << "\")\n";
        }
        cout << "\t(inicio-publicacion  \"" << ini << "\")\n";
        cout << "\t(metodo-distribucion  \"" << metodo << "\")\n";
        cout << "\t(tiene-anime  " << tieneAnime() << ")\n";
        cout << "\t(valoracion  " << valoracion << ")\n";
        cout << "\t(restriccion-edad  " << edad << ")\n";
        cout << ")\n";
    }    
};

bool readManga(Manga& m)
{
    string l;
    int n;

    //Lee titulo
    getline(cin, l);

    m.titulo = l;
    inserta(titulosClips,titulosNormal,l,normToClips(l));

    //Lee autor
    getline(cin, l);
    {
        string aux = normToClips(l);
        inserta(autoresClips,autoresNormal,l,aux);
        m.autor = aux;
    }

    //Lee ilustrador
    getline(cin, l);
    {
        string aux = normToClips(l);
        inserta(autoresClips,autoresNormal,l,aux);
        m.ilustrador = aux;
    }

    //Lee publicadora
    getline(cin, l);
    {
        string aux = normToClips(l);
        if(aux == m.autor or aux == m.ilustrador)
        {
            m.autopublicado = true;
            string aux2 = "publ-";
            aux2.append(aux);
            inserta(autopublicadoresClips,autopublicadoresNormal,l,aux2);
            m.publicadora = aux2;
        }
        else
        {
            m.autopublicado = false;
            inserta(publicadorasClips,publicadorasNormal,l,aux);
            m.publicadora = aux;
        }
    }

    //Lee generos
    cin >> n;
    //Lee n generos
    m.generos = vector<string>(n);
    getline(cin,l);
    for(int i = 0; i < n; ++i)
    {
        getline(cin,m.generos[i]);
        if(not find(m.generos[i],generos))
        {
            cerr << m.generos[i] << "\n";
            __throw_domain_error("genero no reconocido");
        }
    }

    //Lee temas
    cin >> n;
    //Lee n temas
    m.temas = vector<string>(n);
    getline(cin,l);
    for(int i = 0; i < n; ++i)
    {
        getline(cin,m.temas[i]);
        if(not find(m.temas[i],temas))
        {
            cerr << m.temas[i] << "\n";
            __throw_domain_error("tema no reconocido");
        }
    }
    
    //Lee tipo
    cin >> l;
    m.tipo = tipo(l);

    if(m.tipo != "One-shot")
    {
        //Lee tomos
        cin >> n;
        m.tomos = n;
    }

    //Lee capitulos
    cin >> n;
    m.capitulos = n;

    //Lee copias vendidas
    cin >> n;
    m.copias = n;

    //Lee dificultad
    cin >> l;
    m.dificultad = dif(l);

    if(m.tipo != "One-shot")
    {
        //Lee estado publicacion
        cin >> l;
        m.estado = estado(l);

        //Lee frecuencia de publicacion
        cin >> l;
        m.frecuencia = frec(l);
    }

    //Lee fecha inicio
    cin >> l;
    m.ini = l;

    //Lee metodo
    cin >> l;
    m.metodo = metodo(l);

    //Lee anime
    cin >> l;
    l == "y" ? m.anime = true : m.anime = false;

    //Lee valoracion
    {
        float aux;
        cin >> aux;
        m.valoracion = aux;
    }

    //Lee edad
    cin >> n;
    m.edad = n;

    //Guarda datos sobre autores
    if(m.autor == m.ilustrador)
    {
        MangaYAutoria mya;
        mya.nombre = m.titulo;
        mya.escribe = true;
        mya.ilustra = true;
        mangasPorAutor[m.autor].push_back(mya);
    }
    else
    {
        MangaYAutoria myaAutor;
        myaAutor.nombre = m.titulo;
        myaAutor.escribe = true;
        myaAutor.ilustra = false;
        mangasPorAutor[m.autor].push_back(myaAutor);

        MangaYAutoria myaIlustrador;
        myaIlustrador.nombre = m.titulo;
        myaIlustrador.escribe = false;
        myaIlustrador.ilustra = true;
        mangasPorAutor[m.ilustrador].push_back(myaIlustrador);
    }

    getline(cin,l);
    if(getline(cin,l))
        return true;
    return false;
}

void printAutores()
{
    for(auto it = mangasPorAutor.begin(); it != mangasPorAutor.end(); ++it)
    {
        const string& nombreClips = it->first;
        const vector<MangaYAutoria>& mangasClips = it->second;

        cout << "([" << nombreClips << "] of Autor\n";
        cout << "\t(nombre  \"" << autoresNormal[nombreClips] << "\")\n";

        for(const MangaYAutoria& manga : mangasClips)
        {
            if(manga.escribe)
            {
                cout << "\t(escribe  [" << titulosClips[manga.nombre] << "])\n"; 
            }
            if(manga.ilustra)
            {
                cout << "\t(ilustra  [" << titulosClips[manga.nombre] << "])\n";
            }
        }
        cout << ")\n";
    }
}

void printPublicadores()
{
    for(auto it = publicadorasNormal.begin(); it != publicadorasNormal.end(); ++it)
    {
        cout << "([" << it->first << "] of Editorial\n";
        cout << "\t(nombre  \"" << it->second << "\")\n";
        cout << ")\n";
    }

    for(auto it = autopublicadoresNormal.begin(); it != autopublicadoresNormal.end(); ++it)
    {
        cout << "([" << it->first << "] of Autopublicador\n";
        cout << "\t(nombre  \"" << it->second << "\")\n";
        cout << ")\n";
    }
}

void printGeneros()
{
    for(const string& genero : generos)
    {
        cout << "([" << normToClips(genero) << "] of Genero\n";
        cout << "\t(nombre  \"" << genero << "\")\n";
        cout << ")\n";
    }
}

void printTemas()
{
    for(const string& tema : temas)
    {
        cout << "([" << normToClips(tema) << "] of Tema\n";
        cout << "\t(nombre  \"" << tema << "\")\n";
        cout << ")\n";
    }
}

int main()
{
    bool reading = true;

    cout << "(definstances instancias\n";

    do
    {
        Manga m;
        reading = readManga(m);
        m.toClips();
    }
    while(reading);

    cout << "\n; Autores\n\n";
    printAutores();
    cout << "\n; Publicadores\n\n";
    printPublicadores();
    cout << "\n; Generos\n\n";
    printGeneros();
    cout << "\n; Temas\n\n";
    printTemas();

    cout << ")\n";
}