# cancel_cfdi

## Usage
```bash
cancel_cfdi my_pac_name pfxPem pfxPass cfdi_uuid
```

## Environment Variables
The following variables are read when canceling:

**For FEL:** `CFDI_FEL_USER`, `CFDI_FEL_PASS` and `CFDI_FEL_RFC`.

**For ITimbre:** `CFDI_ITIMBRE_USER`, `CFDI_ITIMBRE_PASS` and `CFDI_ITIMBRE_RFC`.

## Exit codes
**0:** Success.

**1:** Environment variable not found.

**2:** Missing argument.

**3:** Unknown PAC.

**4:** Cancel error.
