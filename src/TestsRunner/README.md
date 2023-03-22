# TestsRunner

## Ogrodje za testiranje prevajalnika

Uporaba:

```bash
> make build
> cd .build
> java -cp ".:../lib/*" App test pot_do_prevajalnika pot_do_datoteke_s_testi
```

Če želimo pognati vse teste znotraj direktorija, na koncu poti dodamo zvezdico:

```bash
> java -cp ".:../lib/*" App test pot_do_prevajalnika pot_do_direktorija/*
```

Za dodatne opcije si poglejte definicijo uporabniškega vmesnika v razredu `App.CLI`.