////////////////////////////////////////////////////////////////////////////////
// The following FIT Protocol software provided may be used with FIT protocol
// devices only and remains the copyrighted property of Dynastream Innovations Inc.
// The software is being provided on an "as-is" basis and as an accommodation,
// and therefore all warranties, representations, or guarantees of any kind
// (whether express, implied or statutory) including, without limitation,
// warranties of merchantability, non-infringement, or fitness for a particular
// purpose, are specifically disclaimed.
//
// Copyright 2016 Dynastream Innovations Inc.
////////////////////////////////////////////////////////////////////////////////
// ****WARNING****  This file is auto-generated!  Do NOT edit this file.
// Profile Version = 16.73Release
// Tag = production-akw-16.73.00-0-gef88b3f
////////////////////////////////////////////////////////////////////////////////


#if !defined(FIT_ENCODE_HPP)
#define FIT_ENCODE_HPP

#include <iosfwd>
#include <vector>
#include "fit.h"
#include "fit_mesg.h"
#include "fit_mesg_definition.h"
#include "fit_mesg_definition_listener.h"
#include "fit_mesg_listener.h"

namespace fit
{

class Encode : public MesgListener, public MesgDefinitionListener
{
public:
    Encode(void);
    void Open(std::iostream& file);
    void Write(const MesgDefinition& mesgDef);
    void Write(const Mesg& mesg);
    void Write(const std::vector<Mesg>& mesgs);
    FIT_BOOL Close(void);
    void OnMesg(Mesg& mesg);
    void OnMesgDefinition(MesgDefinition& mesgDef);

private:
    void WriteFileHeader(void);

    MesgDefinition lastMesgDefinition[FIT_MAX_LOCAL_MESGS];
    long dataSize;
    std::iostream *file;
};

} // namespace fit

#endif // defined(FIT_ENCODE_HPP)
