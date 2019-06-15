# stamp

## Usage
```bash
stamp /path/to/csd_pem_file my_pac_name < /path/to/cfdi.xml
```

## Environment Variables
The following variables are read then stamping:

**For FEL:** `STAMP_FEL_USER`, `STAMP_FEL_PASS` and `STAMP_FEL_RFC`.

**For ITimbre:** `STAMP_ITIMBRE_USER`, `STAMP_ITIMBRE_PASS` and `STAMP_ITIMBRE_RFC`.

## Exit codes
**0:** Success.

**1:** Environment variable not found.

**2:** Missing argument.

**3:** Could not parse XML as CFDI.

**4:** Could not sign the CFDI.

**5:** Unknown PAC.

**6:** Stamp error.
