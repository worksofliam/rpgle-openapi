const fs = require('fs');
const path = require('path');

const Structs = require(`./structs`);
const Into = require(`./into`);
const From = require(`./from`);
const Ref = require(`./ref`);

module.exports = class Generator {
    /**
     * @param {object} spec
     * @param {{considerNulls?: boolean, stringLength?: number}} options
     */
    constructor(spec, options) {
        this.spec = spec;
        this.headerFile = [`**FREE`, ``];
        this.prototypes = [``];
        this.baseLines = [`**FREE`, ``];
        this.endLines = [``];

        this.stringLength = options.stringLength || 64;
        this.considerNulls = (options.considerNulls === true);

        Ref.resolve(spec);
    }

    generate() {
        const spec = this.spec;

        if (spec.info) {
            this.baseLines.push(
                `// ${spec.info.title}`,
                `// ${spec.info.description || 'No description available.'}`,
                ``
            );
        }
        
        this.baseLines.push(
            `//*****************************`,
            ``,
            `Ctl-Opt NoMain;`,
            ``,
            `//*****************************`,
            ``,
            `/include './headers/headers.rpgle'`,
            `/include './headers/JSONPARSER.rpgle'`,
            ``,
            `dcl-pr writeJobLog int(10) extproc('Qp0zLprintf');`,
            `  *n pointer value options(*string); // logMsg`,
            `  *n pointer value options(*string:*nopass);`,
            `end-pr;`,
            ``,
            `dcl-pr getenv pointer extproc('getenv');`,
            `  *n pointer value options(*string:*trim);`,
            `end-pr;`,
            ``,
        );
        
        // Custom headers here for all requests
        /* @type {{name: string, description: string}[]} */
        let headers = [];
        
        if (spec.components && spec.components.securitySchemes) {
            for (const key in spec.components.securitySchemes) {
                const scheme = spec.components.securitySchemes[key];
                if (scheme.in === `header`) {
                    headers.push({
                        name: key,
                        header: scheme.name,
                        description: scheme.description,
                    });
                }
            }
        }
        
        this.headerFile.push(
            `Dcl-Ds apiInfo Qualified Template;`,
            `  //@ API endpoint`,
            `  baseUrl varchar(256);`
        );
        
        for (const header of headers) {
            this.headerFile.push(
                `  //@ ${header.description}`,
                `  ${header.name} varchar(256);`,
            );
        }
        
        this.headerFile.push(`End-Ds;`, ``);
        
        this.baseLines.push(
            `//*****************************`,
            ``,
            `Dcl-Proc API_Call;`,
            `  Dcl-Pi API_Call Pointer;`,
            `    //@ API information`,
            `    info LikeDS(apiInfo);`,
            `    //@ API endpoint`,
            `    endpoint Varchar(256);`,
            `    //@ API method.`,
            `    //@ takes get, post, delete, put`,
            `    method varchar(10) const;`,
            `    //@ API body (pass null if not needed)`,
            `    body pointer;`,
            `  End-Pi;`,
            ``,
            `  Dcl-Ds request Qualified;`,
            `    URL    Varchar(512);`,
            `    Header Varchar(1024);`,
            `    Body   Varchar(2048);`,
            `  End-Ds;`,
            ``,
            `  Dcl-S Response     SQLTYPE(CLOB:100000);`,
            `  Dcl-S ResponseJson Pointer;`,
            ``,
            `  request.URL = info.baseUrl + endpoint;`,
            ``,
            `  request.Header `,
            `         = '<httpHeader>'`,
            `         + '<header name="Content-Type" value="application/json" />'`,
            `         + '<header name="Accept-Encoding" value="gzip, deflate" />'`
        );
        
        for (const header of headers) {
            this.baseLines.push(
                `         + '<header name="${header.header}" value="' + info.${header.name} + '" />'`,
            );
        }
        
        this.baseLines.push(
            `         + '</httpHeader>';`,
            ``,
            `  If (body <> *null);`,
            `    request.Body = json_AsText(body);`,
            `  Endif;`,
            ``,
            `  Select;`,
            `    When (method = 'get');`,
            `      EXEC SQL`,
            `        SET :Response = `,
            `        SYSTOOLS.HTTPGETCLOB(`,
            `          :request.URL,`,
            `          :request.Header`,
            `        );`,
            ``,
            `    When (method = 'post');`,
            `      EXEC SQL`,
            `        SET :Response = `,
            `        SYSTOOLS.HTTPPOSTCLOB(`,
            `          :request.URL,`,
            `          :request.Header,`,
            `          :request.Body`,
            `        );`,
            ``,
            `    When (method = 'put');`,
            `      EXEC SQL`,
            `        SET :Response = `,
            `        SYSTOOLS.HTTPPUTCLOB(`,
            `          :request.URL,`,
            `          :request.Header,`,
            `          :request.Body`,
            `        );`,
            ``,
            `    When (method = 'delete');`,
            `      EXEC SQL`,
            `        SET :Response = `,
            `        SYSTOOLS.HTTPDELETECLOB(`,
            `          :request.URL,`,
            `          :request.Header`,
            `        );`,
            `  Endsl;`,
            ``,
            `  json_Close(body);`,
            ``,
            `  If (Response_Len > 0);`,
            `    ResponseJson = json_ParseString(%Subst(Response_Data:1:Response_Len));`,
            ``,
            `    If (getenv('DEBUG_API') <> *null);`,
            `      json_writeJsonStmf(ResponseJson:'/tmp/json-' + %Char(%Timestamp):1208);`,
            `    Endif;`,
            ``,
            `    Return ResponseJson;`,
            `  Else;`,
            `    jl('Call to API has no response: ' + request.URL);`,
            `    Return *null;`,
            `  Endif;`,
            `End-Proc;`,
            ``,
            `//@ Used to determine errors in API calls`,
            `//@ TODO: rename this procedure and the prototype`,
            `Dcl-Proc API_Error Export;`,
            `  Dcl-Pi API_Error Ind End-Pi;`,
            `  Return (sqlstate <> '00000');`,
            `End-Proc;`,
            ``
        );
        
        this.prototypes.push(
            `Dcl-Pr API_Error Ind;`,
            `End-Pr;`,
            ``,
        );
        
        if (spec.paths) {
            for (const path in spec.paths) {
                for (const method in spec.paths[path]) {
                    const route = spec.paths[path][method];
                    const inDs = `${route.operationId}_req`
                    const outDs = `${route.operationId}_res`;
        
                    let reqBody = route.requestBody;
        
                    //Db2 HTTP functions can't handle anything other than successful
                    const responses = Object.keys(route.responses);
                    let resBody = route.responses[responses.find(resp => resp.startsWith("2"))].content;
        
                    if (reqBody) {
                        const requestBody = reqBody.content['application/json'].schema;
                        if (requestBody) {
                            //TODO: structs go into a separate file
                            const inStruct = new Structs();
                            inStruct.generateStruct(requestBody, inDs);
                            this.headerFile.push(...inStruct.lines);
        
                            const dsToJsonProc = new From(this.considerNulls);
                            dsToJsonProc.generateProcedure(requestBody, `${inDs}`);
                            this.endLines.push(...dsToJsonProc.lines);
                        }
                    }
        
                    if (resBody) {
                        if (resBody['application/json']) {
                            const responseBody = resBody['application/json'].schema;
            
                            const outStruct = new Structs();
                            outStruct.generateStruct(responseBody, outDs);
            
                            const jsonToDsProc = new Into();
                            jsonToDsProc.generateProcedure(responseBody, `${outDs}`);
            
                            //TODO: move into own file
                            this.headerFile.push(...outStruct.lines);
            
                            this.endLines.push(...jsonToDsProc.lines);
                        } else {
                            console.log(`${method} ${path} not supported due to lack of JSON response.`);
                            continue;
                        }
                    }
        
                    let queries = [];
                    if (route.parameters) {
                        queries = route.parameters.filter(param => param.in === `query`);
                        queries = queries.map(param => {
                            return {
                                ...param,
                                ...param.schema
                            }});
                    }
        
                    let paramaters = [];
                    let splitUrl = path.split(`/`);
                    for (let i = 0; i < splitUrl.length; i++) {
                        let parm = splitUrl[i]
                        if (parm.startsWith(`{`)) {
                            paramaters.push(parm.substring(1, parm.length - 1));
                            splitUrl[i] = `' + ${parm.substring(1, parm.length - 1)} + '`;
                        }
                    }
                    const pathExpr = splitUrl.join(`/`);
        
                    this.baseLines.push(
                        `//*****************************`,
                        ``,
                        `//@ ${route.operationId}`,
                        `//@ ${route.summary}`,
                        `//@ ${method} ${path}`,
                        `Dcl-Proc ${route.operationId} Export;`,
                        `  Dcl-Pi ${route.operationId}${resBody ? ` LikeDS(${outDs}_t)` : ``};`,
                        `    //@ API information`,
                        `    info LikeDS(apiInfo);`
                    );
        
                    this.prototypes.push(
                        `//@ ${route.operationId}`,
                        `//@ ${route.summary}`,
                        `//@ ${method} ${path}`,
                        `Dcl-Pr ${route.operationId}${resBody ? ` LikeDS(${outDs}_t)` : ``};`,
                        `  //@ API information`,
                        `  info LikeDS(apiInfo);`
                    );
        
                    for (const param of paramaters) {
                        this.baseLines.push(`    ${param} Varchar(${this.stringLength});`);
                        this.prototypes.push(`  ${param} Varchar(${this.stringLength});`);
                    }
        
                    for (const param of queries) {
                        if (param.description) {
                            this.baseLines.push(`    //@ ${param.description.split(`\n`).join(` `)}`);
                            this.prototypes.push(`  //@ ${param.description.split(`\n`).join(` `)}`);
                        }
        
                        if (param.default) {
                            this.baseLines.push(`    //@ default of '${param.default}'`);
                            this.prototypes.push(`  //@ default of '${param.default}'`);
                        }
        
                        if (param.required) {
                            this.baseLines.push(`    //@ Required.`);
                            this.prototypes.push(`    //@ Required.`);
                        } else {
                            this.baseLines.push(`    //@ Optional. Pass ${param.type ? 'blank' : '-999'} to be ignored.`);
                            this.prototypes.push(`    //@ Optional. Pass ${param.type ? 'blank' : '-999'} to be ignored.`);
                        }

                        if (param.minLength && param.maxLength) {
                            if (param.minLength === param.maxLength) {
                                this.baseLines.push(`    //@ Requires exactly ${param.maxLength} characters`);
                                this.prototypes.push(`    //@ Requires exactly ${param.maxLength} characters`);
                            } else {
                                this.baseLines.push(`    //@ Length range: ${param.minLength} <=> ${param.maxLength}.`);
                                this.prototypes.push(`    //@ Length range: ${param.minLength} <=> ${param.maxLength}.`);
                            }
                          }
                          if (param.minimum !== undefined && param.maximum) {
                            this.baseLines.push(`    //@ Numeric range: ${param.minimum} <=> ${param.maximum}.`);
                            this.prototypes.push(`    //@ Numeric range: ${param.minimum} <=> ${param.maximum}.`);
                          }
        
                        switch (param.type) {
                            case `integer`:
                            case `number`:
                                this.baseLines.push(`    ${param.name} Int(10) const;`);
                                this.prototypes.push(`  ${param.name} Int(10) const;`);
                                break;
                            case `boolean`:
                                this.baseLines.push(`    ${param.name} varchar(4) const;`);
                                this.prototypes.push(`  ${param.name} varchar(4) const;`);
                                break;
                            default:
                                this.baseLines.push(`    ${param.name} Varchar(${param.maxLength || 100}) const;`);
                                this.prototypes.push(`  ${param.name} Varchar(${param.maxLength || 100}) const;`);
                                break;
                        }
                    }
        
                    if (reqBody) {
                        this.baseLines.push(`    //@ Request body`);
                        this.baseLines.push(`    body LikeDS(${inDs}_t);`);
                        this.prototypes.push(`  //@ Request body`);
                        this.prototypes.push(`  body LikeDS(${inDs}_t);`);
                    }
        
                    this.baseLines.push(`  End-Pi;`, ``);
                    this.prototypes.push(`End-Pr;`, ``);
        
                    this.baseLines.push(`  Dcl-S endpoint Varchar(256);`);
                    if (reqBody) this.baseLines.push(`  Dcl-S jsonBody Pointer;`);
                    else this.baseLines.push(`  Dcl-S jsonBody Pointer; //Always null`);
                    if (resBody) {
                        this.baseLines.push(
                            `  Dcl-S jsonResponse Pointer;`,
                            `  Dcl-Ds outDs LikeDS(${outDs}_t);`,
                            ``,
                        );
                    }
        
                    this.baseLines.push(
                        `  endpoint = '${pathExpr}?';`,
                        ``
                    );
        
                    for (const parm of queries) {
                        let parmExpr = parm.name;
                        let notEqual = {
                            'integer': '-999',
                            'number': '-999',
                            'boolean': '*BLANK',
                            'string': '*BLANK',
                        }
        
                        switch (parm.type) {
                            case `integer`:
                            case `number`:
                                parmExpr = `%Char(${parm.name})`;
                                break;
                        }
        
                        if (parm.required) {
                            this.baseLines.push(
                                `  // ${parm.name} is not optional.`,
                                `  endpoint = endpoint + '${parm.name}=' + ${parmExpr} + '&';`,
                                ``
                            );
                        } else {
                            this.baseLines.push(
                                `  // ${parm.name} is optional.`,
                                `  if (${parm.name} <> ${notEqual[parm.type]});`,
                                `    endpoint = endpoint + '${parm.name}=' + ${parmExpr} + '&';`,
                                `  endif;`,
                                ``
                            );
                        }
                    }
        
                    if (reqBody) {
                        this.baseLines.push(
                            `  jsonBody = ${inDs}(body);`,
                            ``,
                            `  ${resBody ? `jsonResponse = ` : ``}API_Call(info:endpoint:'${method}':jsonBody);`,
                        );
                    } else {
                        this.baseLines.push(`  ${resBody ? `jsonResponse = ` : ``}API_Call(info:endpoint:'${method}':jsonBody);`,)
                    }
        
                    if (resBody) {
                        this.baseLines.push(
                            ``,
                            `  If (jsonResponse <> *NULL);`,
                            `    outDs = ${outDs}(jsonResponse);`,
                            `  Endif;`,
                            ``,
                            `  return outDs;`,
                        );
                    }
        
                    this.baseLines.push(
                        `End-Proc;`,
                        ``
                    );
                }
            }
        }
        
        this.baseLines.push(
            `//*****************************`,
            ``,
            `Dcl-Proc jl;`,
            `  Dcl-Pi jl;`,
            `    text varchar(200) const;`,
            `  End-pi;`,
            `  dcl-c joblogCRLF const(x'0d25');`,
            `  writeJobLog(text+'%s':joblogCRLF);`,
            `End-Proc;`,
            ``,
            `//*****************************`,
            ...this.endLines
        );
        
        this.headerFile.push(
            ``,
            `//*****************************`,
            ...this.prototypes
        );
    }

    write(directory) {
        fs.writeFileSync(path.join(directory, `base.sqlrpgle`), this.baseLines.join(`\n`));
        fs.writeFileSync(path.join(directory, `headers.rpgle_h`), this.headerFile.join(`\n`));
    }
}