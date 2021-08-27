
//Used to convert a structure to a noxdb schema

module.exports = class From {
  constructor(considerNulls = true) {
    this.considerNulls = considerNulls;
    this.lines = [];
  };

  generateProcedure(object, procedureName) {
    this.lines.push(`Dcl-Proc ${procedureName};`);
    this.lines.push(`  Dcl-Pi *N Pointer;`, `    ${procedureName} LikeDS(${procedureName}_t);`, `  End-Pi;`, ``);
    this.lines.push(`  Dcl-S lIndex Int(5);`);
    this.lines.push(`  Dcl-S lArray Pointer;`);
    this.lines.push(`  Dcl-S lObject Pointer;`);
    this.lines.push(`  Dcl-S lDocument Pointer;`, '');
    this.lines.push(`  lDocument = JSON_NewObject();`);
  
    this.generateContents(object, procedureName, 'lDocument');
  
    this.lines.push(``, `  Return lDocument;`, `End-Proc;`, ``);
  }

  generateContents(object, structName, variable) {
    const setTypes = {
      'number': `SetNum`,
      'integer': 'SetInt',
      'string': 'SetStr',
      'boolean': 'SetBool'
    };

    const nullWhen = {
      'number': '0',
      'integer': '0',
      'string': '*BLANK',
    }
  
    var currentProperty;
    for (var name in object.properties) {
      currentProperty = object.properties[name];
  
      switch (currentProperty.type) {
        case 'string':
          if (currentProperty.nullable && this.considerNulls) this.lines.push(`  If (${structName}.${name} <> ${nullWhen[currentProperty.type]}); //Optional`);
          if (currentProperty.minLength) this.lines.push(`  If (%Len(${structName}.${name}) >= ${currentProperty.minLength});`);
          this.lines.push(`    JSON_${setTypes[currentProperty.type]}(${variable}:'${name}':${structName}.${name});`);
          if (currentProperty.minLength) {
            this.lines.push(
              `  Else;`,
              `    jl('${structName}.${name} requires at least ${currentProperty.minLength} characters');`,
              `  Endif;`
            );
          }
          if (currentProperty.nullable && this.considerNulls) this.lines.push(`  Endif;`);

          break;

        case 'number':
        case 'integer':
          if (currentProperty.nullable && this.considerNulls) this.lines.push(`  If (${structName}.${name} <> ${nullWhen[currentProperty.type]}); //Optional`);

          if (currentProperty.minimum !== undefined && currentProperty.maximum) 
            this.lines.push(`  If (${structName}.${name} >= ${currentProperty.minimum} AND ${structName}.${name} <= ${currentProperty.maximum});`);

          this.lines.push(`    JSON_${setTypes[currentProperty.type]}(${variable}:'${name}':${structName}.${name});`);

          if (currentProperty.minimum !== undefined && currentProperty.maximum) {
            this.lines.push(
              `  Else;`,
              `    jl('${structName}.${name} is not in the range of ${currentProperty.minimum}-${currentProperty.maximum}');`,
              `  Endif;`
            );
          }

          if (currentProperty.nullable && this.considerNulls) this.lines.push(`  Endif;`);
          break;

        case `boolean`:
          this.lines.push(`    JSON_${setTypes[currentProperty.type]}(${variable}:'${name}':${structName}.${name});`);
          break;
  
        case 'object':
          this.lines.push('');
          this.generateContents(currentProperty, structName + "." + name);
          break;
  
        case 'array':
  
          if (currentProperty.items.type === "object") {
            this.lines.push(
              ``,
              `  lArray = JSON_NewArray();`,
              `  For lIndex = 1 to ${structName}.${name}_len;`,
              `    lObject = JSON_NewObject();`
            );

            this.generateContents(currentProperty.items, `${structName}.${name}(lIndex)`, `lObject`);
            
            this.lines.push(
              `    JSON_ArrayPush(lArray:lObject);`,
              `  Endfor;`,
              `  json_SetValue(${variable}:'${name}':lArray:JSON_ARRAY);`
            );
          } else {
            
            this.lines.push(
              ``,
              `  For lIndex = 1 to ${structName}.${name}_len;`,
              `    JSON_${setTypes[currentProperty.items.type]}(${variable}:'${name}[]':${structName}.${name}(lIndex));`,
              `  Endfor;`
            );
          }

          break;
      }
    }
  }
}